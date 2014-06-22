{-|

ARC VIN retrieval webservice hooks.

-}

module AppHandlers.ARC
    (arcImport)

where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import           Data.ByteString as BS (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Conduit
import           Data.Default
import           Data.Functor
import qualified Data.HashMap.Strict as HM
import           Data.Maybe

import           Data.Text as T hiding (map)
import           Data.Text.IO as T
import           Data.Text.Encoding

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

import           Network.Connection
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method

import           Snap (with, getParam)
import           System.Directory
import           System.IO
import           System.IO.Temp

import           Text.XML as XML
import           Text.XML.Cursor as XML

import           Data.Model

import qualified Carma.Model.SubProgram as SubProgram
import qualified Carma.Model.Usermeta as U
import qualified Carma.Model.VinFormat as VF

import           Carma.VIN

import           Application
import           AppHandlers.Util
import qualified Snaplet.DbLayer.Types as DB
import           Util hiding (withPG)


-- | Read @vin@ query parameter and use ARC webservice to load
-- contracts into CaRMa database. Serve JSON list with single integer
-- equal to number of contracts loaded this way.
arcImport :: AppHandler ()
arcImport = do
  sid <- getIntParam "subprogram"
  vin <- fromMaybe "" <$> getParam "vin"

  -- Select a subprogram to load contracts into
  sid' <-
      case sid of
        Just s -> return s
        Nothing -> do
            pid <- fromMaybe (error "No program set") <$> getIntParam "program"
            res <-
                -- If only program is set, select its first subprogram
                -- as the target subprogram, preferring subprograms
                -- with leader=true
                withPG pg_search $ \c -> query c
                [sql|
                 SELECT ? FROM "?" WHERE ?=? ORDER BY ? DESC, ? ASC LIMIT 1;
                 |]
                ( PT $ fieldName SubProgram.ident
                , PT $ tableName $
                  (modelInfo :: ModelInfo SubProgram.SubProgram)
                , PT $ fieldName SubProgram.parent
                , pid
                , PT $ fieldName SubProgram.leader
                , PT $ fieldName SubProgram.ident
                )
            case res of
              (Only i:_) -> return i
              _          -> error $ "No subprograms for program=" ++ show pid

  when (BS.length vin /= 17) $ error "VIN must be 17 characters long"

  connInfo <- with db $ with DB.postgres $ getConnectInfo

  -- Fetch & load
  csv <- liftIO $ parseArc colNames <$> fetchArc vin

  let vinS = B8.unpack vin
  syslogTxt Info "ARC" $ "Obtained good XML for VIN=" ++ vinS

  case csv of
    Nothing -> writeJSON [0::Int]
    Just csv' -> do
      res <- liftIO $ do
        dir <- getTemporaryDirectory
        withTempFile dir "arc.csv" $ \fp fh -> do
          T.hPutStr fh csv'
          hClose fh
          let Ident uid = U.psa
              Ident fid = VF.arc
              opts = Options connInfo fp fp uid fid Nothing (Just sid') True
          syslogTxt Info "ARC" $ "Loading file into subprogram=" ++ show sid'
          doImport opts
      liftIO $ syslogTxt Info "ARC" $ "Loaded file, " ++ show res
      case res of
        Right (ImportResult (_, loaded, _)) -> writeJSON [loaded]
        _ -> error "Could not load VIN into database"


apiUrl :: String
apiUrl = "https://webservices.arceurope.com/vinretrieval/wsvinretrieval.svc/vinretrieval"


authString :: ByteString
authString = "cmFtY193ZWJzZXJ2aWNlczp2NDczMjJySQ=="


-- | Form request body for VIN webservice.
requestText :: ByteString
            -- ^ VIN.
            -> ByteString
requestText vin =
    BS.concat [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
              , "<RequestBody><OutputColumns>"
              , BS.concat $
                map (\c -> BS.concat [ "<column>"
                                     , encodeUtf8 c
                                     , "</column>"]) colNames
              , "</OutputColumns><VIN><VIN_Number>"
              , vin
              , "</VIN_Number></VIN></RequestBody>"
              ]


-- | List of known XML fields in ARC response. This may be obtained
-- from ARC VinFormat as well (field names here match those in
-- VinFormat). However, we include some hacks for parsing XML, so the
-- list is fixed here.
colNames :: [Text]
colNames = [ "DEALER_CODE"
           , "VALID_FROM"
           , "VALID_TO"
           , "VIN_NUMBER"
           , "LICENCE_PLATE_NO"
           , "MAKE"
           , "MODEL"
           , "FIRST_REGISTRATION_DATE"
           ]


-- | Fetch ARC webservice XML response given a VIN. May throw HTTP or
-- XML exception.
fetchArc :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) =>
            ByteString -> m Document
fetchArc vin = do
  url <- parseUrl apiUrl
  let request = url{ requestHeaders = [ ("Authorization", authString)
                                      , ("Content-Type", "text/xml")
                                      ]
                   , requestBody = RequestBodyBS $ requestText vin
                   , method = methodPost
                   }
      -- ARC host has broken certificate, thus we disable server
      -- verification
      settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  withManagerSettings settings $ \m -> do
                 res <- http request m
                 responseBody res $$+- sinkDoc def


-- | Parse ARC XML response to CSV file contents.
parseArc :: [Text]
         -- ^ List of extracted column names.
         -> Document
         -> Maybe Text
parseArc colNames doc = if Prelude.null csvContents
                        then Nothing
                        else Just $ T.unlines $ [csvHeader] ++ csvContents
    where
      root = fromDocument doc
      -- Name with namespace
      myName name = Name{ nameLocalName = name
                           , nameNamespace = ns
                           , namePrefix = Nothing
                           }
      ns = nameNamespace $ elementName $ documentRoot doc
      colNames' = map myName colNames
      vin = T.concat $
            root $| (descendant >=> element (myName "VIN_Number") >=>
                     child >=> content)
      -- From every <Record> element...
      records = root $| (descendant >=> element (myName "Record"))
      -- Select only known children elements
      columns = map (child >=> checkName (flip elem colNames')) records
      -- Extract element names and text contents
      getName n = nameLocalName $ elementName e where
          NodeElement e = node n
      getContent n = T.concat $ n $| (child >=> content)
      mapize col = HM.fromList $ map (\c -> (getName c, getContent c)) col
      csvConcat = T.intercalate ";"
      csvHeader = csvConcat colNames
      rowize recordMap =
          csvConcat $
          -- VIN_NUMBER element is never included in <Record>, but we
          -- already know VIN itself
          map (\k -> HM.lookupDefault (if k == "VIN_NUMBER"
                                       then vin
                                       else "") k recordMap) colNames
      csvContents = map (rowize . mapize) columns
