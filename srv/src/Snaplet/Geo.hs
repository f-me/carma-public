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

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM (unsafeNew, unsafeWrite)

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
routes = [ ("/partners/:coords1/:coords2", method GET $ withinPartners >> return ())
         , ("/distance/:coords1/:coords2", method GET $ distance >> return ())
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
newtype Partner = Partner (Int, Double, Double, Maybe Bool, Maybe Bool) 
                  deriving (FromRow, Show)


instance ToJSON Partner where
    toJSON (Partner (a, b, c, d, e)) = Array $ V.create $ do
                         mv <- VM.unsafeNew 5
                         VM.unsafeWrite mv 0 (toJSON a)
                         VM.unsafeWrite mv 1 (toJSON b)
                         VM.unsafeWrite mv 2 (toJSON c)
                         VM.unsafeWrite mv 3 (toJSON d)
                         VM.unsafeWrite mv 4 (toJSON e)
                         return mv
    {-# INLINE toJSON #-}


------------------------------------------------------------------------------
-- | Read two points from `coords1` and `coords2` request parameters,
-- splice lon1, lat1, lon2 and lat2 on the query and serve the results
-- as JSON, or fail if coordinates could not be read.
twoPointHandler :: (FromRow a, ToJSON r) => 
                   Query
                -> ([a] -> r)
                -- ^ Convert SQL results to a value served in JSON.
                -> Handler b Geo ()
twoPointHandler q queryToResult = do
  c1 <- getCoordsParam "coords1"
  c2 <- getCoordsParam "coords2"

  case (c1, c2) of
    (Just (lon1, lat1), Just (lon2, lat2)) -> do
                   results <- liftM queryToResult $
                              query q (lon1, lat1, lon2, lat2)
                   modifyResponse $ setContentType "application/json"
                   writeLBS $ A.encode results
    _ -> error "Bad request"


------------------------------------------------------------------------------
-- | Query to fetch partners within a box, with mobile partners coming
-- last.
--
-- Splice lon1, lat1 and lon2, lat2 on the query, where coordinates
-- are those of opposite 2D box points.
withinQuery :: Query
withinQuery = "SELECT id, st_x(coords), st_y(coords), isDealer, isMobile FROM partnertbl WHERE coords && ST_SetSRID(ST_MakeBox2D(ST_Point(?, ?), ST_Point(?, ?)), 4326) ORDER BY (case when isMobile then 1 when isMobile is null then 2 else 3 end) DESC;"


------------------------------------------------------------------------------
-- | Query to calculate approximate distance (in meters) between two
-- points.
--
-- Splice lon1, lat1 and lon2, lat2 on the query, where coordinates
-- are those used to measure distance.
distanceQuery :: Query
distanceQuery = "SELECT ST_Distance_Sphere(ST_PointFromText('POINT(? ?)', 4326), ST_PointFromText('POINT(? ?)', 4326));"


------------------------------------------------------------------------------
-- | Serve list of partners within a specified rectangle.
--
-- Response body is a list of 5-tuples @[partner_id, lon, lat,
-- isDealer, isMobile]@. Mobile partners come last.
withinPartners :: Handler b Geo ()
withinPartners = twoPointHandler withinQuery (id :: [Partner] -> [Partner])


------------------------------------------------------------------------------
-- | Calculate distance between two WSG84 points.
--
-- Response body is the distance in meters as double
-- (@6560535.12574021@).
distance :: Handler b Geo ()
distance = twoPointHandler distanceQuery (head . head :: [[Double]] -> Double)


geoInit :: SnapletInit b Geo
geoInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    cfg <- getSnapletUserConfig
    addRoutes routes
    return $ Geo db
