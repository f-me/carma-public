{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Geoservices snaplet.

TODO Use contrib already to switch to JSON and serve HTTP errors.

-}

module Snaplet.Geo
    ( Geo
    , geoInit
    )

where

import Control.Applicative
import Control.Monad

import Control.Monad.State

import Data.Aeson as A

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)

import Data.Lens.Template

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple


data Geo = Geo
    { _postgres :: Snaplet Postgres
    }

makeLens ''Geo


instance HasPostgres (Handler b Geo) where
    getPostgresState = with postgres $ get


routes :: [(ByteString, Handler b Geo ())]
routes = [ ("/partners/:coords/:dist", method GET $ nearbyPartners)
         ]


------------------------------------------------------------------------------
-- | Parse "52.32,3.45" (no spaces) into pair of doubles.
coords :: Parser (Double, Double)
coords = (,) <$> double <* anyChar <*> double


------------------------------------------------------------------------------
-- | Apply a supplied parser to read data from named parameter.
getParamWith :: MonadSnap m => Parser a -> ByteString -> m (Maybe a)
getParamWith parser name = do
  input <- liftM (parseOnly parser) <$> getParam name
  return $ case input of
             Just (Right p) -> Just p
             _ -> Nothing


------------------------------------------------------------------------------
-- | Get pair of coordinates from parameter.
getCoordsParam :: MonadSnap m => ByteString -> m (Maybe (Double, Double))
getCoordsParam = getParamWith coords


------------------------------------------------------------------------------
-- | Row schema for geo_partners table query results.
newtype Partner = Partner (Int, Double, Double) deriving (FromRow, Show, ToJSON)


------------------------------------------------------------------------------
-- | Query to fetch nearby partners.
--
-- Splice lon, lat and distance.
nearbyQuery :: Query
nearbyQuery = "SELECT id, st_x(coords), st_y(coords) FROM geo_partners WHERE ST_DWithin(coords, ST_PointFromText('POINT(? ?)', 4326), ?);"


------------------------------------------------------------------------------
-- | Serve list of partners within a specified distance from a point.
-- Response body is a list of triples @[partner_id, lon, lat]@.
nearbyPartners :: Handler b Geo ()
nearbyPartners = do
  c <- getCoordsParam "coords"
  d <- getParamWith double "dist"

  case (c, d) of
    (Just (lon, lat), Just dist) -> do
                   (results :: [Partner]) <- query nearbyQuery (lon, lat, dist)
                   modifyResponse $ setContentType "application/json"
                   writeLBS $ A.encode results
    _ -> error "Bad request"


geoInit :: SnapletInit b Geo
geoInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    cfg <- getSnapletUserConfig
    addRoutes routes
    return $ Geo db
