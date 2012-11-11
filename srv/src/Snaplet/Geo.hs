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
routes = [ ("/partners/:coords1/:coords2", method GET $ withinPartners)
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
-- | Query to fetch partners within a box.
--
-- Splice lon1, lat1 and lon2, lat2, where coordinates are those of
-- opposite 2D box points.
withinQuery :: Query
withinQuery = "SELECT id, st_x(coords), st_y(coords) FROM geo_partners WHERE coords && ST_SetSRID(ST_MakeBox2D(ST_Point(?, ?), ST_Point(?, ?)), 4326);"


------------------------------------------------------------------------------
-- | Serve list of partners within a specified rectangle.
-- 
-- Response body is a list of triples @[partner_id, lon, lat]@.
withinPartners :: Handler b Geo ()
withinPartners = do
  c1 <- getCoordsParam "coords1"
  c2 <- getCoordsParam "coords2"

  case (c1, c2) of
    (Just (lon1, lat1), Just (lon2, lat2)) -> do
                   (results :: [Partner]) <- query withinQuery (lon1, lat1, lon2, lat2)
                   modifyResponse $ setContentType "application/json"
                   writeLBS $ A.encode results
    _ -> error "Bad request"


geoInit :: SnapletInit b Geo
geoInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    cfg <- getSnapletUserConfig
    addRoutes routes
    return $ Geo db
