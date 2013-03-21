{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-|

Standalone application to update partner coordinates.

-}

module Application
    ( GeoApp
    , geoAppInit)

where

import Control.Applicative
import Control.Lens hiding ((.=), createInstance)
import Control.Monad
import Control.Monad.State hiding (ap)

import Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM

import Data.Attoparsec.ByteString.Char8

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Dict
import qualified Data.Text as T (pack)
import Data.Text.Encoding (encodeUtf8)

import Data.Configurator

import Data.Time.Clock
import Data.Time.Format

import qualified Database.Redis as R
import Database.PostgreSQL.Simple.SqlQQ

import qualified Network.HTTP as H

import System.Locale

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.RedisDB

import Carma.HTTP


data GeoApp = GeoApp
    { _postgres :: Snaplet Postgres
    , _redis :: Snaplet RedisDB
    , carmaPort :: Int
    -- ^ Port of CaRMa instance running on localhost.
    , cityDict :: Dict
    -- ^ Dictionary used to map city names to internal values.
    }

makeLenses ''GeoApp


instance HasPostgres (Handler b GeoApp) where
    getPostgresState = with postgres $ get


routes :: [(ByteString, Handler b GeoApp ())]
routes = [ ("/geo/partner/:pid", method PUT $ updatePosition)
         , ("/geo/partner/:pid", method GET $ getMessage)
         , ("/geo/case/", method POST $ newCase)
         ]


getCarmaPort :: Handler b GeoApp Int
getCarmaPort = gets carmaPort

------------------------------------------------------------------------------
-- | Name of address field in partner model.
partnerAddress :: ByteString
partnerAddress = "addrDeFacto"


------------------------------------------------------------------------------
-- | Name of coordinates field in partner model.
partnerCoords :: ByteString
partnerCoords = "coords"


------------------------------------------------------------------------------
-- | Name of modification time field in partner model.
partnerMtime :: ByteString
partnerMtime = "mtime"


------------------------------------------------------------------------------
-- | Try to obtain city and street address from coordinates using
-- reverse geocoding.
revGeocode :: Double
           -- ^ Longitude.
           -> Double
           -- ^ Latitude.
           -> Handler b GeoApp (Maybe ByteString, Maybe ByteString)
revGeocode lon lat = do
  cp <- getCarmaPort
  (addr :: Maybe (HM.HashMap ByteString ByteString)) <- liftIO $ do
     let coords = (show lon) ++ "," ++ (show lat)
     resp <- H.simpleHTTP $ H.getRequest $
             methodURI cp ("geo/revSearch/" ++ coords)
     body <- H.getResponseBody resp
     return $ decode' $ BSL.pack body
  case addr of
    Just m -> return (HM.lookup "city" m, HM.lookup "address" m)
    Nothing -> return (Nothing, Nothing)


------------------------------------------------------------------------------
-- | Update partner position, setting new address if possible.
updatePosition :: Handler b GeoApp ()
updatePosition = do
  lon' <- getParamWith double "lon"
  lat' <- getParamWith double "lat"
  (pid' :: Maybe Int) <- getParamWith decimal "pid"
  case (lon', lat', pid') of
    (Just lon, Just lat, Just pid) -> do
       addr <- snd <$> revGeocode lon lat
       mtime <- liftIO $ getCurrentTime
       updatePartnerData pid lon lat addr mtime
    _ -> error "Bad request"


------------------------------------------------------------------------------
-- | Send HTTP PUT request to CaRMa API to update partner data.
updatePartnerData :: Int
                  -- ^ Partner id.
                  -> Double
                  -- ^ Longitude.
                  -> Double
                  -- ^ Latitude.
                  -> (Maybe ByteString)
                  -- ^ New address if available.
                  -> UTCTime
                  -- ^ New partner mtime.
                  -> Handler b GeoApp ()
updatePartnerData pid lon lat addr mtime =
    let
        coordString = concat [show lon, ",", show lat]
        body = HM.fromList $
               [ (partnerCoords, BS.pack coordString)
               , (partnerMtime,
                  BS.pack $ formatTime defaultTimeLocale "%s" mtime)] ++
               (maybe [] (\a -> [(partnerAddress, a)]) addr)
    in do
      cp <- getCarmaPort
      liftIO $ updateInstance cp "partner" pid body >> return ()


------------------------------------------------------------------------------
-- | Used to fetch messages for a partner with id supplied as a query
-- parameter.
getMessageQuery :: Query
getMessageQuery = [sql|
SELECT message FROM partnerMessageTbl
WHERE partnerId=? order by ctime desc limit 1;
|]


getMessage :: Handler b GeoApp ()
getMessage = do
  Just pid <- getParam "pid"
  let partnerId = BS.append "partner:" pid
  res <- query getMessageQuery $ Only partnerId
  case res of
    (Only msg):_ -> writeLBS msg
    _ -> writeLBS "{}"


------------------------------------------------------------------------------
-- | Name of case coordinates field in case model.
caseCoords :: ByteString
caseCoords = "caseAddress_coords"


------------------------------------------------------------------------------
-- | Name of case address field in case model.
caseAddress :: ByteString
caseAddress = "caseAddress_address"


------------------------------------------------------------------------------
-- | Name of case city field in case model.
caseCity :: ByteString
caseCity = "city"


------------------------------------------------------------------------------
-- | Name of case id field in action model.
actionCaseId :: ByteString
actionCaseId = "caseId"


------------------------------------------------------------------------------
-- | Name of actions field in case model.
caseActions :: ByteString
caseActions = "actions"

------------------------------------------------------------------------------
-- | Build reference to a case for use in 'actionCaseId'.
caseIdReference :: Int -> ByteString
caseIdReference n = BS.pack $ "case:" ++ (show n)


------------------------------------------------------------------------------
-- | Build reference to an action for use in 'caseActions'.
actionIdReference :: Int -> ByteString
actionIdReference n = BS.pack $ "action:" ++ (show n)


------------------------------------------------------------------------------
-- | Create a new case from a JSON object provided in request body.
-- Perform reverse geocoding using coordinates from values under @lon@
-- and @lat@, writing the obtained city and street address to the new
-- case. New @callMeMaybe@ action is created for the case.
--
-- Response body is a JSON of form @{"caseId":<n>}@, where @n@ is the
-- new case id.
newCase :: Handler b GeoApp ()
newCase = do
  -- New case parameters
  rqb <- readRequestBody 4096
  let Just jsonRq = Aeson.decode rqb
      Just jsonRq0 = Aeson.decode rqb
      coords = parseMaybe (\j -> (,) <$> (j .:"lon") <*> (j .: "lat")) jsonRq0
      car_vin = HM.lookup "car_vin" jsonRq

  dict <- gets cityDict

  jsonRq' <- case coords of
    Nothing -> return jsonRq
    -- Reverse geocode coordinates from lon/lat
    Just (lon,lat) -> revGeocode lon lat >>= \case
      (addr, city) ->
        return $
        (maybe id (HM.insert caseCity) $ city >>= (flip valueOfLabel dict)) $
        (maybe id (HM.insert caseAddress) addr) $
        HM.insert caseCoords (BS.pack $ concat [show lon, ",", show lat]) $
        jsonRq

  -- Form the body of the new case request to send to CaRMa
  let caseBody =  HM.delete "lon" $
                  HM.delete "lat" $
                  HM.delete "car_vin" $ -- we'll insert it later to run trigger
                  jsonRq'

  modifyResponse $ setContentType "application/json"
  cp <- getCarmaPort
  caseResp <- liftIO $ createInstance cp "case" caseBody

  now <- liftIO $ getCurrentTime
  let nowStr = BS.pack $ formatTime defaultTimeLocale "%s" (now :: UTCTime)
  -- Fetch id of the created case and create a new action for this id
  let caseId = fst caseResp
      descr = encodeUtf8 $ T.pack
            $  "Клиент заказал услугу с помощью мобильного приложения. "
            ++ "Требуется перезвонить ему и уточнить детали"
      actBody = HM.fromList
                [ (actionCaseId, caseIdReference caseId)
                , ("name", "callMeMaybe")
                , ("targetGroup", "back")
                , ("duetime", nowStr)
                , ("priority", "1")
                , ("closed", "0")
                , ("description", descr)
                ]
  actResp <- liftIO $ createInstance cp "action" actBody

  -- Fetch id of the created action and update the reverse reference
  -- in the case.
  let actId = fst actResp
  liftIO $ updateInstance cp "case" caseId $ HM.fromList $
          [ (caseActions, actionIdReference actId) ]
          -- we update car_vin here to trigger vin-search (it's a bit
          -- easier than adding correct trigger handling on POST
          -- request)
          ++ maybe [] (\vin -> [("car_vin", vin)]) car_vin

  writeLBS . encode $ object $ [ "caseId" .= show caseId ]


geoAppInit :: SnapletInit b GeoApp
geoAppInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    rdb <- nestSnaplet "redis" redis $ redisDBInit R.defaultConnectInfo
    cfg <- getSnapletUserConfig

    cp <- liftIO $ lookupDefault 8000 cfg "carma_port"
    dName <- liftIO $ lookupDefault "DealerCities" cfg "cities-dictionary"

    addRoutes routes
    cDict' <- liftIO $ readDictionary cp dName
    case cDict' of
      Just cDict -> return $ GeoApp db rdb cp cDict
      Nothing -> error "Could not load cities dictionary from CaRMa"


------------------------------------------------------------------------------
-- | Apply a parser to read data from a named request parameter.
getParamWith :: MonadSnap m =>
                Parser a
             -> ByteString
             -- ^ Parameter name.
             -> m (Maybe a)
getParamWith parser name = do
  input <- liftM (parseOnly parser) <$> getParam name
  return $ case input of
             Just (Right p) -> Just p
             _ -> Nothing
