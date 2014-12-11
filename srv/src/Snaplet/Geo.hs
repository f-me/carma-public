{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-|

Geoservices snaplet:

- Nominatim forward/reverse geocoding

- distance calculation

- geospatial partner search

All coordinates read by various handlers from request parameters are
in WSG84 in @longitude,latitude@ format, as @33.77,52.128@.

-}

module Snaplet.Geo
    ( Geo(..)
    , geoInit
    )

where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.State hiding (state)

import Data.Aeson as A

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Map as M
import Data.HashMap.Strict as HM (delete)
import Data.Maybe as Maybe

import Data.Configurator
import Database.PostgreSQL.Simple.SqlQQ
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP as H (simpleHTTP, getRequest, getResponseBody, urlEncode)

import Snap.Core
import Snap.Extras.JSON
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

import AppHandlers.Util (getParamT)
import Util


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
    getPostgresState = with postgres get


routes :: [(ByteString, Handler b Geo ())]
routes = [ ("/partners/:coords1/:coords2", method GET withinPartners)
         , ("/distance/:coords1/:coords2", method GET distance)
         , ("/revSearch/:coords", method GET revSearch)
         , ("/search/:query", method GET search)
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
-- last. See 'withinPartners' and `geowithin` SQL stored procedure.
--
-- Splice 13 parameters, starting with lon1, lat1 and lon2, lat2 on
-- the query, where coordinates are those of opposite 2D box points.
withinQuery :: Query
withinQuery = [sql|
SELECT geowithin(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
-- @coord1@ and @coord2@. Coordinates in @from@ param define central
-- point from which distances are calculated (optional). Partners
-- maybe be filteres with @city@, @make@, @services@, @priority2@ and
-- @priority3@, @isDealer@, @mobilePartner@ params.
--
-- Response body is a list of JSON objects, representing partners
-- joined with partner_service (see @withinQuery@ for list of fields).
-- Mobile partners come last.
withinPartners :: Handler b Geo ()
withinPartners = do
  c1 <- getCoordsParam "coords1"
  c2 <- getCoordsParam "coords2"
  c <- getCoordsParam "from"

  city <- fromMaybe ""  <$> getParam "city[]"
  make <- fromMaybe ""  <$> getParam "make[]"
  srv  <- fromMaybe ""  <$> getParam "services[]"
  pr2  <- fromMaybe ""  <$> getParam "priority2"
  pr3  <- fromMaybe ""  <$> getParam "priority3"
  dlr  <- fromMaybe "0" <$> getParam "isDealer"
  mp   <- fromMaybe "0" <$> getParam "mobilePartner"

  let (centered, lonc, latc) =
          case c of
            Just (lon, lat) -> (True, lon, lat)
            _               -> (False, 0, 0)

  case (c1, c2) of
    (Just (lon1, lat1), Just (lon2, lat2)) -> do
        let qParams = ()
                      :* lon1 :* lat1 :* lon2 :* lat2
                      :* lonc :* latc
                      :* city :* make
                      :* srv  :* pr2 :* pr3
                      :* dlr  :* mp
        results <- recode <$> query withinQuery qParams
        -- Do not serve useless distance if center point is not cet
        let results' = if centered
                       then results
                       else Prelude.map (HM.delete "distance") results
        modifyResponse $ setContentType "application/json"
        writeLBS $ A.encode results'
    _ -> error "Bad request"
    where
      recode :: [[BSL.ByteString]] -> [A.Object]
      recode = Maybe.mapMaybe A.decode . Prelude.concat


------------------------------------------------------------------------------
-- | Calculate distance between two points specified in request
-- parameters @coord1@ and @coord2@. Response body is the distance in
-- meters as double:
--
-- > /distance/37.144775245113,55.542910552955/38.140411231441,56.006347982652/
-- > 80825.169705850
distance :: Handler b Geo ()
distance = twoPointHandler distanceQuery (head . head :: [[Double]] -> Double)


------------------------------------------------------------------------------
-- | True only for names of Russian cities which are federal subjects
-- (in UTF-8, ru-RU).
isFederal :: Text -> Bool
isFederal s = s == "Москва" || s == "Санкт-Петербург" || s == "Севастополь"


------------------------------------------------------------------------------
-- | City and street address. 'FromJSON' instance parses a UTF-8
-- response from the Nominatim reverse geocoder, properly handling
-- federal city names.
data FullAddress = FullAddress (Maybe Text) (Maybe Text)
                   deriving Show


instance FromJSON FullAddress where
    parseJSON (Object v) = do
        err <- v .:? "error"
        case (err :: Maybe Text) of
          Just _ -> fail "Geocoding failed"
          Nothing -> do
            addr <- v .: "address"
            road  <- addr .:? "road"         .!= Nothing
            ped   <- addr .:? "pedestrian"   .!= Nothing
            city  <- addr .:? "city"         .!= Nothing
            state <- addr .:? "state"        .!= Nothing
            house <- addr .:? "house_number" .!= Nothing
            let -- Use road/pedestrian fields to pick the street name
                street = case (road, ped) of
                           (r, Nothing) -> r
                           (Nothing, p) -> p
                           _ -> Nothing
                -- Include house number information in the street
                -- address, if present
                streetAddr = case (street, house) of
                               (Just s, Just h) -> Just $ T.concat [s, ", ", h]
                               _ -> street
                -- Use the name of the state as the city name for
                -- federal cities
                realCity = case (city, state) of
                             (c, Just s) -> if isFederal s
                                            then Just s
                                            else c
                             _ -> city
            return $ FullAddress realCity streetAddr
    parseJSON _ = fail "Bad Nominatim response"


instance ToJSON FullAddress where
    toJSON (FullAddress c s) = object [ "city" .= c
                                      , "address" .= s
                                      ]


------------------------------------------------------------------------------
-- | Use Nominatim to perform a reverse search for an address at
-- coordinates provided in @coords@ request parameter. Response body
-- is a JSON object with keys @city@ and @address@ (possibly with null
-- values), or a single key @error@ if Nominatim geocoding failed
-- completely.
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
                      "&lon=" ++ show lon ++
                      "&lat=" ++ show lat
        addr' <- liftIO $ do
            rsb <- simpleHTTP (H.getRequest fullUrl) >>= getResponseBody
            return $ eitherDecode' $ BSL.pack rsb
        -- Repack Nominatim response into a nicer JSON
        case addr' of
          Right addr -> writeJSON (addr :: FullAddress)
          Left msg -> writeJSON (M.singleton ("error" :: String) msg)


-- | Use Nominatim to perform a search for objects at an address
-- provided in @query@ request parameter. Response body is identical
-- to Nominatim's original response.
search :: Handler b Geo ()
search = do
  nom <- gets nominatimUrl
  lang <- gets nominatimLang
  q <- getParamT "query"
  case q of
    Nothing -> error "Empty query"
    Just q' -> do
      let --qD = fromMaybe (error "Bad query encoding") $ urlDecode q'
          fullUrl = nom ++ "search?format=json" ++
                    "&accept-language=" ++ lang ++
                    "&q=" ++ (H.urlEncode $ T.unpack q')
      rsb <- liftIO $ simpleHTTP (H.getRequest fullUrl) >>= getResponseBody
      writeLBS $ BSL.pack rsb


geoInit :: SnapletInit b Geo
geoInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    cfg <- getSnapletUserConfig
    nom <- liftIO $ lookupDefault "http://nominatim.openstreetmap.org/"
           cfg "nominatim-url"
    lang <- liftIO $ lookupDefault "ru-RU,ru" cfg "nominatim-lang"
    addRoutes routes
    return $ Geo db nom lang
