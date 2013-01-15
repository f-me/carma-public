{-# LANGUAGE OverloadedStrings #-}

{-|

  CLI tool for CaRMa partner import.

  Loads tab-separated CSV files to partner database.

 -}

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Aeson as A
import Data.Attoparsec.Char8

import Data.Dict
import Data.Either
import Data.List
import qualified Data.Map as M

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import qualified Data.CSV.Conduit as CSV
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Data.Text as T
import Data.Text.Encoding

import Network.HTTP
import Network.URI (parseURI)

import System.Environment
import System.Exit
import System.IO


type Row = CSV.MapRow BS.ByteString

type FieldValue = BS.ByteString

type FieldName = BS.ByteString


-- | Set of CaRMa dictionaries which parametrize the import process.
data IntegrationDicts =
    IntegrationDicts { cityDict    :: Dict
                     -- ^ Dictionary of allowed city names.
                     , carDict     :: Dict
                     -- ^ Dictionary of allowed car models.
                     , taxDict     :: Dict
                     -- ^ Dictionary of allowed tax schemes.
                     , servDict    :: Dict
                     -- ^ Dictionary of services supported by the
                     -- importer.
                     }


-- | Provides access to integration dictionaries when building row
-- processors.
type IntegrationMonad = Reader IntegrationDicts


loadIntegrationDicts :: FilePath
                     -> FilePath
                     -> FilePath
                     -> FilePath
                     -> IO (Maybe IntegrationDicts)
loadIntegrationDicts cityFile carFile taxFile servFile = do
  cityDict'    <- loadDict cityFile
  carDict'     <- loadDict carFile
  taxDict'     <- loadDict taxFile
  servDict'    <- loadDict servFile
  return $ liftM4 IntegrationDicts cityDict' carDict' taxDict' servDict'


data RowError = UnknownId
              -- ^ No partner with this id is found.
              | MissingColumns [FieldName]
              -- ^ Some of row columns are not present
              | FieldError FieldErrorType FieldName FieldValue
              -- ^ An error occured when processing a field.
                deriving Show


data FieldErrorType = BadPhone
                    -- ^ Could not read phone number from a field.
                    | BadWorkingTime
                    -- ^ Could not read working time from a field.
                    | UnknownCity
                    | UnknownCar
                    | UnknownTaxScheme
                    | UnknownService
                      deriving (Show, Eq, Ord)


-- | Shorthand for 'encodeUtf8'.
e8 :: T.Text -> BS.ByteString
e8 = encodeUtf8


-- | Translate 'RowError' to readable message.
--
-- Remember, no quotes.
formatError :: RowError -> BS.ByteString
formatError UnknownId =
    e8 "Дилер не найден в системе, попробуйте очистить Id и загрузить его снова"
formatError (MissingColumns cols) =
    BS.concat [ e8 "В записи отсутствуют обязательные поля: "
              , BS.intercalate ", " cols
              ]
formatError (FieldError UnknownCity _ _) =
    e8 "На наших картах нет такого города"
formatError (FieldError et fn fv) =
    BS.concat [msg, fn, ": ", fv]
    where
      msg = case et of
              BadPhone -> e8 "Недопустимый формат телефона в поле "
              _        -> e8 "Недопустимое значение в поле "


-- | Pure row processor, possibly yielding a processing error for this row.
type RowProcessor = Row -> Either RowError Row


-- | Pure field processor which returns Nothing when processing fails.
type FieldProcessor = FieldValue -> Maybe FieldValue


-- | Prepend address with city.
cityToAddress :: FieldName
              -- ^ Take city from this field...
              -> FieldName
              -- ^ .. and prepend this field with it.
              -> RowProcessor
cityToAddress cityField addressField =
    \row ->
        let
            city = row M.! cityField
            address = row M.! addressField
            newAddress =
                BS.concat [e8 "г. ", city, e8 ", ", address]
        in
          Right $ M.insert addressField newAddress row


-- | Format phone value.
phoneField :: FieldProcessor
phoneField phone =
    let
        phone'  = B8.filter isDigit phone
    in
      case B8.length phone' of
        0  -> Just phone'
        10 -> Just $ B8.concat ["+7", phone']
        11 -> case (B8.head phone', B8.tail phone')  of
                ('8', rst) -> Just $ B8.concat ["+7", rst]
                ('7', _)   -> Just $ B8.cons '+' phone'
                _          -> Nothing
        _  -> Nothing


-- | Parser used to match working time given in format
-- @HH:MM-HH:MM/D1-D2;HH:MM-HH:MM/D1-D2;..@.
wtFormat :: Parser ()
wtFormat =
    let
        dash     = char '-'
        hourmins = ((digit >> digit) <|> digit) >>  char ':' >> (digit >> digit)
        daytime  = hourmins                     >>  dash     >> hourmins
        weekdays = (digit >> dash >> digit)     <|> digit
        singleWT = daytime                      >>  char '/' >> weekdays
    in
      sepBy1 singleWT (char ';') >> return ()


-- | Replace en and em dashes (Unicode @\x2013@, @\x2014@) with hyphen
-- (@\x2d@).
replaceDashes :: FieldValue -> FieldValue
replaceDashes = e8 .
                (T.replace "–" "-") .
                (T.replace "—" "-") .
                decodeUtf8


-- | Ignore spaces, replace dashes with with hyphens. Return clean
-- value or Nothing if a field value fails to match the expected
-- working time format.
wtField :: FieldProcessor
wtField workingTime =
    let
        workingTime' = replaceDashes $
                       B8.filter (not . isSpace) workingTime
    in
      if B8.null workingTime'
      then Just workingTime'
      else
          case parseOnly wtFormat workingTime' of
            Right _ -> Just workingTime'
            Left _  -> Nothing


-- | If field value is a valid dictionary label, replace it with
-- internal dictionary value of that label.
dictField :: Dict -> FieldProcessor
dictField dict = \fieldValue -> valueOfLabel fieldValue dict


-- | Make a list of row processors from mapping between field names,
-- processors (applied to field values) and error types (used when
-- respective processors fail). Thus, only single-field processors are
-- supported.
--
-- Processor constructors are actions in 'IntegrationMonad' to provide
-- access to integration dictionaries.
--
-- All fields are assumed to be present in every row.
buildProcessors :: [(FieldName,
                     IntegrationMonad FieldProcessor,
                     FieldErrorType)]
                -> IntegrationMonad [RowProcessor]
buildProcessors procdef =
    mapM (\(fieldName, procSpec, errType) -> do
            fieldProc <- procSpec
            return $ \row ->
                let
                    fieldValue = row M.! fieldName
                in
                  case fieldProc fieldValue of
                    Just result ->
                        Right $ M.insert fieldName result row
                    Nothing ->
                        Left $ FieldError errType fieldName fieldValue)
    procdef


-- | Name of partner id field in CSV file.
csvIdField :: FieldName
csvIdField = "Id"


csvErrorField :: FieldName
csvErrorField = e8 "Ошибка"


-- | Mapping between CSV column names and fields of partner model in
-- CaRMa.
--
-- We may implement this as an external dictionary (shall we?).
carmaFieldMapping :: [(FieldName, FieldName)]
carmaFieldMapping =
    [ (e8 "Название Дилера", "name")
    , (e8 "Код дилера", "code")
    , (e8 "Город", "city")
    , (e8 "Марка", "makers")
    , (e8 "Адрес сервисного отдела", "serviceAddress")
    , (e8 "Телефон сервисного отдела", "servicePhone")
    , (e8 "Время работы cервисного отдела", "serviceWorking")
    , (e8 "Адрес Отдела продаж", "salesAddress")
    , (e8 "Телефон Отдела продаж", "salesPhone")
    , (e8 "Время работы Отдела продаж", "salesWorking")
    , (e8 "Юридический адрес Офиса", "addrDeJure")
    , (e8 "Фактический адрес Офиса", "addrDeFacto")
    , (e8 "Факс", "fax")
    , (e8 "Ответственное лицо за Assistance", "personInCharge")
    , (e8 "Контактный телефон ответственного за Assistance", "closeTicketPhone")
    , (e8 "Еmail ответственного за Assistance", "closeTicketEmail")
    , (e8 "Форма налогообложения", "taxScheme")
    , (e8 "Услуга (техпомощь / эвакуатор / техпомощь и эвакуатор)", "services")
    , (e8 "Телефон для заказа Услуги", "phone1")
    , (e8 "Время работы по предоставлению услуги", "workingTime")
    , (e8 "Комментарии", "comment")
    ]


carmaConstFields :: [(FieldName, FieldValue)]
carmaConstFields =
    [ ("isPayBackConfirmed", "0")
    , ("isMobile", "0")
    , ("isActive", "1")
    , ("isDealer", "1")
    ]


fieldValidationProcessors :: IntegrationMonad [RowProcessor]
fieldValidationProcessors =
    let
        phoneFields = [ "Телефон сервисного отдела"
                      , "Телефон Отдела продаж"
                      , "Факс"
                      , "Контактный телефон ответственного за Assistance"
                      , "Телефон для заказа Услуги"
                      ]
        wtFields =    [ "Время работы cервисного отдела"
                      , "Время работы Отдела продаж"
                      , "Время работы по предоставлению услуги"
                      ]
    in
      buildProcessors $
      map (\n -> (e8 n, pure phoneField, BadPhone)) phoneFields ++
      map (\n -> (e8 n, pure wtField, BadWorkingTime)) wtFields ++
      
      -- Label-to-value conversions for dictionary fields
      [ ( e8 "Город"
        , dictField <$> asks cityDict
        , UnknownCity
        )
      , ( e8 "Марка"
        , dictField <$> asks carDict
        , UnknownCar
        )
      , ( e8 "Форма налогообложения"
        , dictField <$> asks taxDict
        , UnknownTaxScheme
        )
      , ( e8 "Услуга (техпомощь / эвакуатор / техпомощь и эвакуатор)"
        , dictField <$> asks servDict
        , UnknownService
        )
      ]

-- | List of processors to be applied prior to writing a processed row
-- to output CSV.
mkValidationProcessors :: IntegrationMonad [RowProcessor]
mkValidationProcessors =
    let
        pureProcessors = [ cityToAddress (e8 "Город") (e8 "Адрес сервисного отдела")
                         ]
    in do
      fv <- fieldValidationProcessors
      return $ pureProcessors ++ fv


-- | A processor which renames field names according to a mapping.
-- Only keys mentioned in the mapping are used to build the result
-- row. If some keys are missing from the row, return 'MissingColumns'
-- error.
remappingProcessor :: [(FieldName, FieldName)] -> RowProcessor
remappingProcessor keyMapping row =
    let
        remapRes = map (\(csvName, internalName) ->
                        case M.lookup csvName row of
                          Just val -> Right (internalName, val)
                          Nothing -> Left csvName
                       )
                   keyMapping
        (missingColumns, results) = partitionEithers remapRes
    in
      if null missingColumns
      then Right $ M.fromList results
      else Left $ MissingColumns missingColumns


-- | A processor which sets field values according to provided
-- mapping. Never fails.
fieldSetterProcessor :: [(FieldName, FieldValue)] -> RowProcessor
fieldSetterProcessor fieldVals startingRow =
    Right $ foldl' (\row (fn, fv) -> M.insert fn fv row) startingRow fieldVals


-- | Sequentially apply all row processors, accumulating errors.
applyProcessors :: [RowProcessor] -> Row -> (Row, [RowError])
applyProcessors procs startingRow =
    foldl' (\(row, errs) rowProcessor ->
                case rowProcessor row of
                  Right newRow -> (newRow, errs)
                  Left  newErr -> (row, errs ++ [newErr]))
           (startingRow, [])
           procs


-- | If at least one row processor yields an error satisfying this
-- predicate, no CaRMa request will be sent in 'processRow'.
isCritical :: RowError -> Bool
isCritical UnknownId                    = True
isCritical (MissingColumns _)           = True
isCritical (FieldError UnknownCity _ _) = True
isCritical _                            = False


-- | True if row error is caused by 'UnknownService'.
isUnknownService :: RowError -> Bool
isUnknownService (FieldError UnknownService _ _) = True
isUnknownService _                               = False


-- | Update dependant service model instances and references between
-- services and parent partner model.
updateRowServices :: Int
               -- ^ CaRMa port.
               -> Int 
               -- ^ Partner ID
               -> Row
               -- ^ Processed partner data from CSV (must have @services@
               -- field).
               -> IO ()
updateRowServices cp pid row = do
  -- Fetch requires service types from @services@ field of row
  let servs = (B8.split ',' $ row M.! "services") ++ ["rent"]
  -- Create service instances
  servIds <- forM servs (createService cp pid)
  -- Write service IDs to partner
  let servRef = B8.intercalate "," $
                map (\i -> B8.pack $ "partner_service:" ++ (show i)) servIds
  _ <- updatePartner cp pid (M.singleton "services" servRef)
  return ()


-- | Check id, then sequentially apply a list of processors to row. If
-- no id for row is set, apply an extra list of processors for new
-- rows. Create/update new partners in CaRMa and pass processing
-- results further along the pipe.
--
-- We use keyless 'CSV.Row' as output to maintain initial column
-- order.
processRow :: MonadResource m =>
              [RowProcessor]
           -- ^ CSV row processors.
           -> [RowProcessor]
           -- ^ Processors applied to new partners only.
           -> [FieldName]
           -- ^ Column order for CSV output.
           -> Int
           -- ^ CaRMa port.
           -> CSV.MapRow BS.ByteString
           -> m (CSV.Row BS.ByteString)
processRow procs newProcs columnOrder cp freshRow = liftIO $ do
  -- Start by trying to read integer value from "id" field to
  -- determine mode of operation for this row (create/update).
  let pidS = freshRow M.! csvIdField
      maybePid = liftM fst $ B8.readInt pidS
  (pid, pidErrs) <-
      case maybePid of
        Nothing -> return (Nothing, [])
        Just n -> do
          -- If partner id is set, check if it really exists in the
          -- system.
          res <- partnerExists cp n
          return $ case res of
                     True -> (Just n, [])
                     False -> (Nothing, [UnknownId])

  -- Apply processors, gather and format errors.
  let (processedRow, procErrs) = applyProcessors procs freshRow
      allErrs = pidErrs ++ procErrs
      formattedErrs = BS.intercalate "; " $ map formatError allErrs

  -- If no critical processing errors occured, send partner data to CaRMa.
  carmaPid <- case (all (not . isCritical) allErrs) of
                False -> return Nothing
                True  -> Just <$>
                    case pid of
                      Just n  -> updatePartner cp n processedRow
                      -- Additionally newProcs when creating new
                      -- partners
                      Nothing -> createPartner cp $
                                 fst $
                                 applyProcessors newProcs processedRow

  -- Create services for a new partner using value of "services" field
  -- (must be remapped from CSV field by now; all services must be
  -- recognized using the services dictionary). Due to impurity this
  -- cannot be implemented as a row processor.
  case (pid, carmaPid, any isUnknownService allErrs) of
    (Nothing, Just n, False) -> updateRowServices cp n processedRow
    _ -> return ()

  -- Add formatted errors list to output CSV row. If CaRMa request was
  -- performed, set new value of partner id. Note that we use
  -- freshRow, so the rest of fields are left unchanged.
  let outRow = M.insert csvErrorField formattedErrs $
               M.insert csvIdField
                    (maybe BS.empty (B8.pack . show) carmaPid) $
               freshRow

  return $ orderMapRow columnOrder outRow


-- | Convert 'CSV.MapRow' to 'CSV.Row' using a provided column
-- ordering. All columns must be present in the source row.
orderMapRow :: Ord a =>
               [a]
            -> CSV.MapRow a
            -> CSV.Row a
orderMapRow columnOrder sourceRow = map ((M.!) sourceRow) columnOrder


-- | CaRMa JSON response containing "id" field. The rest of fields are
-- ignored.
--
-- TODO: carma-mobile-server contains this code too.
newtype IdResponse = IdResponse Int deriving Show

instance FromJSON IdResponse where
    parseJSON (Object v) = IdResponse . read <$> v .: "id"
    parseJSON _          = error "Bad CaRMa response"


-- | Model create API endpoint.
modelURI :: Int
           -- ^ CaRMa port.
         -> String
         -- ^ Model name.
         -> String
modelURI cp model = concat ["http://localhost:", show cp, "/_/", model, "/"]


-- | Model read/update API endpoint.
modelPidURI :: Int -> String -> Int -> String
modelPidURI cp model pid = (modelURI cp model) ++ (show pid)


-- | Create/update model using row data and return its id.
modelRequest :: Int
             -- ^ CaRMa port.
             -> String
             -- ^ Model name.
             -> Maybe Int
             -- ^ Model id.
             -> RequestMethod
             -> Row
             -- ^ Request payload.
             -> IO Int
modelRequest cp model pid rm row = do
  let uri =
          case pid of
            Just n  -> modelPidURI cp model n
            Nothing -> modelURI cp model
  rs <- simpleHTTP $
        mkRequestWithBody uri rm "application/json" (BSL.unpack $ encode row)
  rsBody <- getResponseBody rs
  let Just (IdResponse carmaPid) =
          case pid of
            Just n -> Just (IdResponse n)
            Nothing -> decode' (BSL.pack rsBody) :: Maybe IdResponse
  return carmaPid


createPartner :: Int -> Row -> IO Int
createPartner cp = modelRequest cp "partner" Nothing POST


createService :: Int 
              -> Int
              -- ^ Parent partner id.
              -> BS.ByteString
              -- ^ Value of @serviceName@ field for service.
              -> IO Int
createService cp pid srv =
    modelRequest cp "partner_service" Nothing POST $
    M.insert "parentId" (B8.pack $ "partner:" ++ (show pid)) $
    M.insert "serviceName" srv $
    M.empty


updatePartner :: Int -> Int -> Row -> IO Int
updatePartner cp pid = modelRequest cp "partner" (Just pid) PUT


-- | Derived from 'postRequestWithBody' from HTTP package.
mkRequestWithBody :: String
                  -- ^ URL string.
                  -> RequestMethod
                  -> String
                  -- ^ Content-type header value.
                  -> String
                  -- ^ Request body.
                  -> Request_String
mkRequestWithBody urlString method typ body =
  case parseURI urlString of
    Nothing -> error ("putRequestWithBody: Not a valid URL - " ++ urlString)
    Just u  -> setRequestBody (mkRequest method u) (typ, body)

-- | Check if partner exists in the CaRMa database.
partnerExists :: Int -> Int -> IO Bool
partnerExists cp pid = do
  rs <- simpleHTTP $ getRequest $ modelPidURI cp "model" pid
  code <- getResponseCode rs
  return $ case code of
             (2, 0, 0) -> True
             (4, 0, 4) -> False
             _ -> error "Unexpected CaRMa response when querying for partner data"


-- | Default settings for partner list CSV files: semicolon-separated
-- fields, quoted.
csvSettings :: CSV.CSVSettings
csvSettings = CSV.CSVS ';' (Just '"') (Just '"') ';'


usage :: String
usage = "Usage: " ++
        "partner-import 8000 dealers.csv out.csv " ++
        "DealerCities.json PSACarMakers.json TaxSchemes.json PSAServices.json"


main :: IO ()
main = do
  args <- getArgs
  when (length args /= 7) $ putStrLn usage >> exitFailure

  -- Load dictionaries and use them to build concrete row processors
  let (cp:input:output:cityFile:carFile:taxFile:servFile:_) = args
  Just dicts <- loadIntegrationDicts cityFile carFile taxFile servFile

  let processors = runReader mkValidationProcessors dicts ++
                   [remappingProcessor carmaFieldMapping]
      newPartnerProcessors = [fieldSetterProcessor carmaConstFields]
      carmaPort = read cp


  -- Read head row to find out column order
  Just headRow <- runResourceT $
       sourceFile input $=
       CSV.intoCSV csvSettings $$
       CL.head :: IO (Maybe (CSV.Row BS.ByteString))

  runResourceT $ yield headRow $= CSV.fromCSV csvSettings $$ sinkFile output

  -- We use sinkHandle to *append* processing results to header row.
  outHandle <- openFile output AppendMode
  runResourceT $
       sourceFile input $=
       CSV.intoCSV csvSettings $=
       (CL.mapM $ processRow processors newPartnerProcessors headRow carmaPort) $=
       (CSV.fromCSV csvSettings) $$
       (sinkHandle outHandle)
  hClose outHandle
