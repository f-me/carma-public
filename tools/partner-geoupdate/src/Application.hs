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
import Control.Monad.State

import Data.Aeson

import Data.Attoparsec.ByteString.Char8

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text.Encoding (decodeUtf8)

import Data.Configurator

import Data.Lens.Template

import qualified Database.Redis as R

import qualified Network.HTTP as H
import Network.URI (parseURI)

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
-- | URI for PUT request to update partner data.
partnerUpdateURI :: Int -> Handler b GeoApp String
partnerUpdateURI pid = do
  cp <- gets carmaPort
  return $ concat ["http://localhost:", show cp, "/_/partner/", show pid]


------------------------------------------------------------------------------
-- | Name of address field in partner model.
addressField :: ByteString
addressField = "addrDeFacto"


------------------------------------------------------------------------------
-- | Name of coordinates field in partner model.
coordsField :: ByteString
coordsField = "coords"


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
-- | Update partner position, setting new address if possible.
updatePosition :: Handler b GeoApp ()
updatePosition = do
  lon' <- getParamWith double "lon"
  lat' <- getParamWith double "lat"
  (pid' :: Maybe Int) <- getParamWith decimal "pid"
  case (lon', lat', pid') of
    (Just lon, Just lat, Just pid) -> do
        nomU <- nominatimRevURI lon lat
        -- Reverse geocode street address from coordinates.
        (addr :: Maybe Address) <- liftIO $ do
          resp <- H.simpleHTTP (H.getRequest nomU)
          body <- H.getResponseBody resp
          return $ decode' $ BSL.pack body
        updatePartnerData pid lon lat addr
    _ -> error "Bad request"



-- | Send HTTP PUT request to CaRMa API to update partner data.
updatePartnerData :: Int
                  -- ^ Partner id.
                  -> Double
                  -- ^ Longitude.
                  -> Double
                  -- ^ Latitude.
                  -> (Maybe Address)
                  -- ^ New address if available.
                  -> Handler b GeoApp ()
updatePartnerData pid lon lat addr =
    let
        coordString = concat [show lon, ",", show lat]
        body = BSL.unpack $ encode $ object $
               [decodeUtf8 coordsField .= coordString] ++
               (maybe [] (\(Address a) -> [decodeUtf8 addressField .= a]) addr)
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
