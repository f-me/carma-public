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

import Data.Lens.Template

import qualified Database.Redis as R

import qualified Network.HTTP as H

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.RedisDB


data GeoApp = GeoApp
    { _postgres :: Snaplet Postgres
    , _redis :: Snaplet RedisDB
    }

makeLens ''GeoApp


instance HasPostgres (Handler b GeoApp) where
    getPostgresState = with postgres $ get


routes :: [(ByteString, Handler b GeoApp ())]
routes = [ ("/geo/partner/:pid", method PUT $ updatePosition)
         ]


------------------------------------------------------------------------------
-- | Build reverse geocoding request URI.
nominatimRevQuery :: Double -> Double -> String
nominatimRevQuery lon lat = 
    concat ["http://nominatim.openstreetmap.org/reverse.php?"
           , "format=json&accept-language=ru-RU,ru&"
           , "lon=", show lon, "&"
           , "lat=", show lat
           ]


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
-- | Name of address field in partner model.
addressField :: ByteString
addressField = "addrDeFacto"


------------------------------------------------------------------------------
-- | Set new address to partner with given id.
writeAddress :: Int -> Address -> R.Redis ()
writeAddress pid (Address addr) = do
  let key = BS.concat ["partner:", BS.pack . show $ pid]
  R.hset key addressField addr >> return ()


------------------------------------------------------------------------------
-- | Query to update partner position.
--
-- Splice with lon, lat and partner id.
updateQuery :: Query
updateQuery = "UPDATE geo_partners SET coords=ST_PointFromText('POINT(? ?)', 4326) WHERE id=?;"


------------------------------------------------------------------------------
-- | Update partner position, setting new address if possible.
updatePosition :: Handler b GeoApp ()
updatePosition = do
  lon' <- getParamWith double "lon"
  lat' <- getParamWith double "lat"
  (id' :: Maybe Int) <- getParamWith decimal "pid"
  case (lon', lat', id') of
    (Just lon, Just lat, Just id) -> do
        execute updateQuery (lon, lat, id)

        -- Reverse geocode street address from coordinates.
        (addr' :: Maybe Address) <- liftIO $ do
          resp <- H.simpleHTTP (H.getRequest $ nominatimRevQuery lon lat)
          body <- H.getResponseBody resp
          return $ decode' $ BSL.pack body
        case addr' of
          Just addr -> runRedisDB redis $ writeAddress id addr
          _ -> return ()
    _ -> error "Bad request"


geoAppInit :: SnapletInit b GeoApp
geoAppInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    rdb <- nestSnaplet "redis" redis $ redisDBInit R.defaultConnectInfo
    cfg <- getSnapletUserConfig
    addRoutes routes
    return $ GeoApp db rdb


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
