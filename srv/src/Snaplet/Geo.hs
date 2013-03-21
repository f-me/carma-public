{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-|

Geoservices snaplet.

All coordinates read by various handlers from request parameters are
in WSG84 in @<longitude>,<latitude>@ format, as @33.77,52.128@.

-}

module Snaplet.Geo
    ( Geo
    , geoInit
    )

where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.State

import Data.Aeson as A

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as BS (ByteString, concat)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Configurator
import Database.PostgreSQL.Simple.SqlQQ
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM (unsafeNew, unsafeWrite)

import Network.HTTP as H (simpleHTTP, getRequest, getResponseBody)

import Snap.Core
import Snap.Extras.JSON
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple


data Geo = Geo
    { _postgres     :: Snaplet Postgres
    , nominatimUrl  :: String
    -- ^ Nominatim installation URL (with trailing slash).
    , nominatimLang :: String
    -- ^ Preferred language for search results (in RFC 2616
    -- Accept-Language format, or a comma-separated list of language
    -- codes).
    }

makeLenses ''Geo


instance HasPostgres (Handler b Geo) where
    getPostgresState = with postgres $ get


routes :: [(ByteString, Handler b Geo ())]
routes = [ ("/partners/:coords1/:coords2", method GET withinPartners >> return ())
         , ("/distance/:coords1/:coords2", method GET distance >> return ())
         , ("/revSearch/:coords", method GET revSearch)
         ]


------------------------------------------------------------------------------
-- | Parse "52.32,3.45" (no spaces) into pair of doubles.
coords :: Parser (Double, Double)
coords = (,) <$> double <* anyChar <*> double


------------------------------------------------------------------------------
-- | Apply a parser to read data from a named request parameter.
getParamWith :: MonadSnap m => Parser a -> ByteString -> m (Maybe a)
getParamWith parser name = do
  input <- liftM (parseOnly parser) <$> getParam name
  return $ case input of
             Just (Right p) -> Just p
             _ -> Nothing


------------------------------------------------------------------------------
-- | Get a pair of coordinates from a named request parameter.
getCoordsParam :: MonadSnap m => ByteString -> m (Maybe (Double, Double))
getCoordsParam = getParamWith coords


------------------------------------------------------------------------------
-- | Row schema for 'withinQuery' results.
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
                -- ^ Converts SQL results to a value served in JSON.
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
-- last. See 'withinPartners'.
--
-- Splice lon1, lat1 and lon2, lat2 on the query, where coordinates
-- are those of opposite 2D box points.
withinQuery :: Query
withinQuery = [sql|
SELECT id, st_x(coords), st_y(coords), isDealer, isMobile
FROM partnertbl
WHERE coords && ST_SetSRID(ST_MakeBox2D(ST_Point(?, ?), ST_Point(?, ?)), 4326)
ORDER BY
(case when isMobile then 1 when isMobile is null then 2 else 3 end)
DESC;
|]


------------------------------------------------------------------------------
-- | Query to calculate approximate distance (in meters) between two
-- points. See 'distance'.
--
-- Splice lon1, lat1 and lon2, lat2 on the query, where coordinates
-- are those used to measure distance.
distanceQuery :: Query
distanceQuery = [sql|
SELECT ST_Distance_Sphere(ST_PointFromText('POINT(? ?)', 4326),
                          ST_PointFromText('POINT(? ?)', 4326));
|]


------------------------------------------------------------------------------
-- | Serve a list of partners located within a rectangle given by
-- coordinates of two opposite points specified in request parameters
-- @coord1@ and @coord2@.
--
-- Response body is a JSON list of 5-tuples @[partner_id, lon, lat,
-- isDealer, isMobile]@. Mobile partners come last.
withinPartners :: Handler b Geo ()
withinPartners = twoPointHandler withinQuery (id :: [Partner] -> [Partner])


------------------------------------------------------------------------------
-- | Calculate distance between two points specified in request
-- parameters @coord1@ and @coord2@. Response body is the distance in
-- meters as double:
--
-- > /distance/37.144775245113,55.542910552955/38.140411231441,56.006347982652/
-- 80825.169705850
distance :: Handler b Geo ()
distance = twoPointHandler distanceQuery (head . head :: [[Double]] -> Double)


------------------------------------------------------------------------------
-- | True only for names of Russian cities which are federal subjects
-- (in UTF-8, ru-RU).
isFederal :: ByteString -> Bool
isFederal s = (s == e8 "Москва") || (s == e8 "Санкт-Петербург")
    where
      e8 = encodeUtf8


------------------------------------------------------------------------------
-- | City and street address. 'FromJSON' instance parses a UTF-8
-- response from the Nominatim reverse geocoder, properly handling
-- federal city names.
data FullAddress = FullAddress (Maybe ByteString) (Maybe ByteString) 
                   deriving Show


instance FromJSON FullAddress where
    parseJSON (Object v) = do
        (err::Maybe ByteString) <- v .:? "error"
        case err of
          Just _ -> fail "Geocoding failed"
          Nothing -> do
            addr <- v .: "address"
            state <- addr .: "state"
            road <- addr .:? "road" .!= Nothing
            ped <- addr .:? "pedestrian" .!= Nothing
            city <- addr .:? "city" .!= Nothing
            house <- addr .:? "house_number" .!= Nothing
            let -- Use road/pedestrian fields to pick the street name
                street = case (road, ped) of
                           (r, Nothing) -> r
                           (Nothing, p) -> p
                           _ -> Nothing
                -- Include house number information in the street
                -- address, if present
                streetAddr = case (street, house) of
                               (Just s, Just h) -> Just $ BS.concat [s, ", ", h]
                               _ -> street
                -- Use the name of the state as the city name for
                -- federal cities
                realCity = if isFederal state
                           then Just state
                           else city
            return $ FullAddress realCity streetAddr
    parseJSON _ = fail "Bad Nominatim response"


instance ToJSON FullAddress where
    toJSON (FullAddress c s) = object [ "city" .= c
                                      , "address" .= s
                                      ]


------------------------------------------------------------------------------
-- | Use Nominatim to perform a reverse search for an address at
-- coordinates provided in @coords@ request parameter. Response body
-- is a JSON object with keys @city@ and @address@.
revSearch :: Handler b Geo ()
revSearch = do
  nom <- gets nominatimUrl
  lang <- gets nominatimLang
  coords' <- getCoordsParam "coords"
  case coords' of
    Nothing -> error "Bad request"
    -- Read coords and send reverse geocoding request to Nominatim
    Just (lon, lat) -> do
        let fullUrl = nom ++
                      "reverse.php?format=json" ++
                      "&accept-language=" ++ lang ++
                      "&lon=" ++ (show lon) ++
                      "&lat=" ++ (show lat)
        addr' <- liftIO $ do
            rsb <- simpleHTTP (H.getRequest fullUrl) >>= getResponseBody
            return $ eitherDecode' $ BSL.pack rsb
        -- Repack Nominatim response into a nicer JSON
        case addr' of
          Right addr -> writeJSON (addr :: FullAddress)
          Left msg -> error msg


geoInit :: SnapletInit b Geo
geoInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    cfg <- getSnapletUserConfig
    nom <- liftIO $ lookupDefault "http://nominatim.openstreetmap.org/"
           cfg "nominatim-url"
    lang <- liftIO $ lookupDefault "ru-RU,ru" cfg "nominatim-lang"
    addRoutes routes
    return $ Geo db nom lang
