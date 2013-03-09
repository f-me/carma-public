{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Standalone application to update partner coordinates.

-}

module Application
    ( GeoApp
    , geoAppInit)

where

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (ap)

import Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM

import Data.Attoparsec.ByteString.Char8

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T (pack)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Data.Configurator

import Data.Time.Clock
import Data.Time.Format

import Data.Lens.Template

import qualified Database.Redis as R

import qualified Network.HTTP as H
import Network.URI (parseURI)

import System.Locale

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.RedisDB


data GeoApp = GeoApp
    { _postgres :: Snaplet Postgres
    , _redis :: Snaplet RedisDB
    , nominatimReverse :: String
    -- ^ URI to Nominatim reverse.php up to first ?
    , carmaPort :: Int
    -- ^ Port of CaRMa instance running on localhost.
    }

makeLens ''GeoApp


instance HasPostgres (Handler b GeoApp) where
    getPostgresState = with postgres $ get


routes :: [(ByteString, Handler b GeoApp ())]
routes = [ ("/geo/partner/:pid", method PUT $ updatePosition)
         , ("/geo/partner/:pid", method GET $ getMessage)
         , ("/geo/case/", method POST $ newCase)
         ]


------------------------------------------------------------------------------
-- | URI for reverse geocoding request.
nominatimRevURI :: Double -> Double -> Handler b GeoApp String
nominatimRevURI lon lat = do
  nh <- gets nominatimReverse
  return $
    concat [nh
           , "format=json&accept-language=ru-RU,ru&"
           , "lon=", show lon, "&"
           , "lat=", show lat
           ]


------------------------------------------------------------------------------
-- | URI for POST/PUT requests to create or update a new case.
caseCreateUpdateURI :: (Maybe Int)
                    -- ^ Case ID.
                    -> Handler b GeoApp String
caseCreateUpdateURI cid' = do
  cp <- gets carmaPort
  return $ concat ["http://localhost:", show cp, "/_/case/", cid]
  where
    cid = maybe "" show cid'


------------------------------------------------------------------------------
-- | URI for POST request to create a new action
actionCreateURI ::Handler b GeoApp String
actionCreateURI = do
  cp <- gets carmaPort
  return $ concat ["http://localhost:", show cp, "/_/action/"]


------------------------------------------------------------------------------
-- | URI for PUT request to update partner data.
partnerUpdateURI :: Int -> Handler b GeoApp String
partnerUpdateURI pid = do
  cp <- gets carmaPort
  return $ concat ["http://localhost:", show cp, "/_/partner/", show pid]


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
-- | Address line as parsed from Nominatim reverse geocoder JSON
-- response.
newtype Address = Address ByteString deriving Show


instance FromJSON Address where
    parseJSON (Object v) = Address <$> do
        (err::Maybe String) <- v .:? "error"
        case err of
          Just _ -> fail "Geocoding failed"
          Nothing -> do
            addr <- v .: "address"
            road <- addr .: "road"
            house <- addr .:? "house_number" .!= Nothing
            return $ case house of
                       Just h -> BS.concat [road, ", ", h]
                       Nothing -> road
    parseJSON _ = fail "Bad Nominatim response"

instance ToJSON Address where
    toJSON (Address s) = String $ decodeUtf8 s


------------------------------------------------------------------------------
-- | Attempt to perform reverse geocoding.
revGeocode :: Double
           -- ^ Longitude.
           -> Double
           -- ^ Latitude.
           -> Handler b GeoApp (Maybe Address)
revGeocode lon lat = do
  nomU <- nominatimRevURI lon lat
  (addr :: Maybe Address) <- liftIO $ do
     resp <- H.simpleHTTP (H.getRequest nomU)
     body <- H.getResponseBody resp
     return $ decode' $ BSL.pack body
  return addr


------------------------------------------------------------------------------
-- | Update partner position, setting new address if possible.
updatePosition :: Handler b GeoApp ()
updatePosition = do
  lon' <- getParamWith double "lon"
  lat' <- getParamWith double "lat"
  (pid' :: Maybe Int) <- getParamWith decimal "pid"
  case (lon', lat', pid') of
    (Just lon, Just lat, Just pid) -> do
       addr <- revGeocode lon lat
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
                  -> (Maybe Address)
                  -- ^ New address if available.
                  -> UTCTime
                  -> Handler b GeoApp ()
updatePartnerData pid lon lat addr mtime =
    let
        coordString = concat [show lon, ",", show lat]
        body = BSL.unpack $ encode $ object $
               [decodeUtf8 partnerCoords .= coordString] ++
               [decodeUtf8 partnerMtime .= formatTime defaultTimeLocale "%s" mtime] ++
               (maybe [] (\(Address a) -> [decodeUtf8 partnerAddress .= a]) addr)
    in do
      parU <- partnerUpdateURI pid
      liftIO $ H.simpleHTTP
           (putRequestWithBody parU "application/json" body)
      return ()


getMessageQuery :: Query
getMessageQuery = "SELECT message FROM partnerMessageTbl where partnerId=? order by ctime desc limit 1;"


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
caseCoords :: Text
caseCoords = "caseAddress_coords"


------------------------------------------------------------------------------
-- | Name of case address field in case model.
caseAddress :: Text
caseAddress = "caseAddress_address"


------------------------------------------------------------------------------
-- | Name of case id field in action model.
actionCaseId :: Text
actionCaseId = "caseId"


------------------------------------------------------------------------------
-- | Name of actions field in case model.
caseActions :: Text
caseActions = "actions"

------------------------------------------------------------------------------
-- | Build reference to a case for use in 'actionCaseId'.
caseIdReference :: Int -> String
caseIdReference n = "case:" ++ (show n)


------------------------------------------------------------------------------
-- | Build reference to an action for use in 'caseActions'.
actionIdReference :: Int -> String
actionIdReference n = "action:" ++ (show n)


------------------------------------------------------------------------------
-- | CaRMa JSON response containing "id" field. The rest of fields are
-- ignored.
newtype IdResponse = IdResponse Int deriving Show


instance FromJSON IdResponse where
    parseJSON (Object v) = IdResponse . read <$> v .: "id"
    parseJSON _          = error "Bad CaRMa response"


------------------------------------------------------------------------------
-- | Create a new case from JSON in request body. Perform reverse
-- geocoding using coordinates from values under @lon@ and @lat@,
-- writing the result to field 'caseAddress' of the new case. New
-- action is created for the case.
--
-- Response body is a JSON of form @{"caseId":<n>}@, where @n@ is the
-- new case id.
newCase :: Handler b GeoApp ()
newCase = do
  -- New case parameters
  Just jsonRq <- Aeson.decode <$> readRequestBody 4096

  let coords = parseMaybe (\j -> (,) <$> (j .:"lon") <*> (j .: "lat")) jsonRq
  let car_vin = parseMaybe (.: "car_vin") jsonRq

  jsonRq' <- case coords of
    Nothing -> return jsonRq
    -- Reverse geocode coordinates from lon/lat
    Just (lon,lat) -> revGeocode lon lat >>= \case
      Nothing -> return jsonRq
      Just addr -> return
        $ HM.insert caseAddress (toJSON addr)
        $ HM.insert caseCoords  (String $ T.pack $ concat [show lon, ",", show lat])
        $ jsonRq

  -- Form the body of the new case request to send to CaRMa
  let caseBody = BSL.unpack $ encode
               $ HM.delete "lon"
               $ HM.delete "lat"
               $ HM.delete "car_vin" -- we'll insert it later to run trigger
               $ jsonRq'

  modifyResponse $ setContentType "application/json"
  caseU <- caseCreateUpdateURI Nothing
  caseResp <- liftIO $ H.simpleHTTP
          (H.postRequestWithBody caseU "application/json" caseBody)

  now <- liftIO $ getCurrentTime
  let nowStr = formatTime defaultTimeLocale "%s" (now :: UTCTime)
  -- Fetch id of the created case and create a new action for this id
  caseRespBody <- liftIO $ H.getResponseBody caseResp
  let Just (IdResponse caseId) = decode' (BSL.pack caseRespBody) :: Maybe IdResponse
      descr = T.pack
            $  "Клиент заказал услугу с помощью мобильного приложения. "
            ++ "Требуется перезвонить ему и уточнить детали"
      actBody = BSL.unpack $ encode $ object $
                [ actionCaseId .= caseIdReference caseId
                , "name" .= ("callMeMaybe" :: Text)
                , "targetGroup" .= ("back" :: Text)
                , "duetime" .= nowStr
                , "priority" .= ("1" :: Text)
                , "closed"   .= ("0" :: Text)
                , "description" .= descr
                ]
  actU <- actionCreateURI
  actResp <- liftIO $ H.simpleHTTP
             (H.postRequestWithBody actU "application/json" actBody)

  -- Fetch id of the created action and update reverse reference in the case.
  actRespBody <- liftIO $ H.getResponseBody actResp
  let Just (IdResponse actId) = decode' (BSL.pack actRespBody) :: Maybe IdResponse
  caseU' <- caseCreateUpdateURI (Just caseId)
  liftIO $ H.simpleHTTP $
         putRequestWithBody caseU' "application/json" $ BSL.unpack $ encode $ object $
          [ caseActions .= actionIdReference actId ]
          -- we update car_vin here to trigger vin-search
          -- (it's a bit easier than adding correct trigger handling on POST request)
          ++ maybe [] (\vin -> ["car_vin" .= (vin :: Text)]) car_vin

  writeLBS . encode $ object $ [ "caseId" .= show caseId ]


geoAppInit :: SnapletInit b GeoApp
geoAppInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    rdb <- nestSnaplet "redis" redis $ redisDBInit R.defaultConnectInfo
    cfg <- getSnapletUserConfig

    nh <- liftIO $ lookupDefault
          "http://nominatim.openstreetmap.org/reverse.php?"
          cfg
          "nominatim_reverse"

    cp <- liftIO $ lookupDefault 8000 cfg "carma_port"

    addRoutes routes
    return $ GeoApp db rdb nh cp


------------------------------------------------------------------------------
-- | Use the supplied parser to read data from request parameter.
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


------------------------------------------------------------------------------
-- | Derived from 'postRequestWithBody' from HTTP package.
putRequestWithBody :: String -> String -> String -> H.Request_String
putRequestWithBody urlString typ body =
  case parseURI urlString of
    Nothing -> error ("putRequestWithBody: Not a valid URL - " ++ urlString)
    Just u  -> H.setRequestBody (H.mkRequest H.PUT u) (typ, body)
