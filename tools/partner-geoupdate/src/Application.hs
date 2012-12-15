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

import Data.Aeson

import Data.Attoparsec.ByteString.Char8

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T (pack)
import Data.Text.Encoding (decodeUtf8)

import Data.Configurator

import Data.Time.Clock
import Data.Time.Format

import Data.Lens.Template
import Data.Maybe

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
-- | Derived from 'postRequestWithBody' from HTTP package.
putRequestWithBody :: String -> String -> String -> H.Request_String
putRequestWithBody urlString typ body =
  case parseURI urlString of
    Nothing -> error ("putRequestWithBody: Not a valid URL - " ++ urlString)
    Just u  -> H.setRequestBody (H.mkRequest H.PUT u) (typ, body)


getMessageQuery :: Query
getMessageQuery = "SELECT message FROM partnerMessageTbl where partnerId=? order by ctime desc limit 1;"


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


getMessage :: Handler b GeoApp ()
getMessage = do
  Just pid <- getParam "pid"
  let partnerId = BS.append "partner:" pid
  res <- query getMessageQuery $ Only partnerId
  case res of
    (Only msg):_ -> writeLBS msg
    _ -> writeLBS "{}"


------------------------------------------------------------------------------
-- | POST parameters expected in a new case request.
caseParams :: [ByteString]
caseParams = [ "contact_name"
             , "contact_phone1"
             , "car_vin"
             , "car_make"
             , "car_model"
             , "car_plateNum"
             , "car_color"
             , "car_buyDate"
             ]

caseCoords = "caseAddress_coords"

caseAddress = "caseAddress_address"


------------------------------------------------------------------------------
-- | Create new case from POST parameters listed in 'caseParams'.
-- Reverse geocode coordinates from @lon@ and @lat@ parameters,
-- writing the result to field 'caseAddress' of the new case.
newCase :: Handler b GeoApp ()
newCase = do
  -- New case parameters
  rawPairs <- forM caseParams $ 
               \key -> do
                 v' <- getParam key
                 return $ ((.=) $ decodeUtf8 key) <$> v'

  lon' <- getParamWith double "lon"
  lat' <- getParamWith double "lat"

  -- Reverse geocode coordinates from lon/lat
  (addrPair, coordPair) <- case (lon', lat') of
    (Just lon, Just lat) -> do
            addr' <- revGeocode lon lat
            let ap = ((.=) $ decodeUtf8 caseAddress) <$> addr'
                cp = Just $ (decodeUtf8 caseCoords) .= 
                     (T.pack $ concat [show lon, ",", show lat])
            return (ap, cp)
    _ -> return (Nothing, Nothing)
         
  -- Form the body of the request to send to CaRMa
  let finalPairs = rawPairs ++ [addrPair, coordPair]
      caseBody = BSL.unpack $ encode $ object $ catMaybes finalPairs

  modifyResponse $ setContentType "application/json"
  caseU <- caseCreateUpdateURI Nothing
  resp <- liftIO $ H.simpleHTTP 
          (H.postRequestWithBody caseU "application/json" caseBody)
  body <- liftIO $ H.getResponseBody resp
  writeLBS $ BSL.pack $ show body


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
