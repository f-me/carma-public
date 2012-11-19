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

import Control.Monad

import Control.Monad.State

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B

import Data.Functor

import Data.Lens.Template

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple


data GeoApp = GeoApp
    { _postgres :: Snaplet Postgres
    }

makeLens ''GeoApp


instance HasPostgres (Handler b GeoApp) where
    getPostgresState = with postgres $ get


routes :: [(ByteString, Handler b GeoApp ())]
routes = [ ("/geo/partner/:pid", method PUT $ updatePosition)
         , ("/geo/partner/:pid", method GET $ getMessage)
         ]


------------------------------------------------------------------------------
-- | Query to update partner position.
--
-- Splice with lon, lat and partner id.
updateQuery :: Query
updateQuery = "UPDATE geo_partners SET coords=ST_PointFromText('POINT(? ?)', 4326) WHERE id=?;"


getMessageQuery :: Query
getMessageQuery = "SELECT message FROM partnerMessageTbl where partnerId=? order by ctime desc limit 1;"

------------------------------------------------------------------------------
-- | Update partner position.
updatePosition :: Handler b GeoApp ()
updatePosition = do
  lon' <- getParamWith double "lon"
  lat' <- getParamWith double "lat"
  (id' :: Maybe Int) <- getParamWith decimal "pid"
  case (lon', lat', id') of
    (Just lon, Just lat, Just id) ->
        execute updateQuery (lon, lat, id) >> return ()
    _ -> error "Bad request"

getMessage :: Handler b GeoApp ()
getMessage = do
  Just id <- getParam "pid"
  let partnerId = B.append "partner:" id
  res <- query getMessageQuery $ Only partnerId
  case res of
    (Only msg):_ -> writeLBS msg
    _ -> writeLBS "{}"

geoAppInit :: SnapletInit b GeoApp
geoAppInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    cfg <- getSnapletUserConfig
    addRoutes routes
    return $ GeoApp db


------------------------------------------------------------------------------
-- | Apply a supplied parser to read data from named parameter.
getParamWith :: MonadSnap m => Parser a -> ByteString -> m (Maybe a)
getParamWith parser name = do
  input <- liftM (parseOnly parser) <$> getParam name
  return $ case input of
             Just (Right p) -> Just p
             _ -> Nothing
