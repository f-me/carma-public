{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

{-|

Geoservices snaplet:

- Nominatim forward/reverse geocoding (@\/search\/:query@ and
@\/revSearch\/:coords@)

- distance calculation (@\/distance\/:coords1\/:coords2@)

- geospatial partner search (@\/partners\/:coords1\/:coords2@)

All coordinates read by various handlers from request parameters are
in WSG84 in @longitude,latitude@ format, as @33.77,52.128@.

-}

module Snaplet.Geo
    ( Geo(..)
    , geoInit
    )

where

import Control.Lens hiding ((.=), withLens)
import Control.Monad.State hiding (state)

import Data.Aeson as A

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as BS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder as BS (toLazyByteString)
import Data.List (isSuffixOf)
import Data.Map as M
import Data.HashMap.Strict as HM (delete)
import Data.Maybe as Maybe
import Data.Monoid ((<>))

import Data.Configurator
import Database.PostgreSQL.Simple.SqlQQ
import Data.Text (Text)
import qualified Data.Text as T
import Text.InterpolatedString.QM

import qualified Network.HTTP.Conduit as HttpC
import qualified Network.HTTP.Types.URI as HttpT

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

import Util


data Geo b = Geo
    { postgres      :: SnapletLens b Postgres
    , nominatimReq  :: String -> IO HttpC.Request
    -- ^ Nominatim installation URL (with trailing slash).
    , nominatimLang :: T.Text
    -- ^ Preferred language for search results (in RFC 2616
    -- Accept-Language format, or a comma-separated list of language
    -- codes).
    }

makeLenses ''Geo


routes :: [(ByteString, Handler b (Geo b) ())]
routes = [ ("/partners/:coords1/:coords2", method GET withinPartners)
         , ("/distance/:coords1/:coords2", method GET distance)
         , ("/revSearch/:coords",          method GET revSearch)
         , ("/search/:query",              method GET search)
         ]


------------------------------------------------------------------------------
-- | Parse "52.32,3.45" (no spaces) into pair of doubles.
coords :: Parser (Double, Double)
coords = (,) <$> double <* anyChar <*> double


------------------------------------------------------------------------------
-- | Get a pair of coordinates from a named request parameter.
getCoordsParam :: MonadSnap m => ByteString -> m (Maybe (Double, Double))
getCoordsParam = parseMayParam coords


------------------------------------------------------------------------------
-- | Read two points from `coords1` and `coords2` request parameters,
-- splice lon1, lat1, lon2 and lat2 on the query and serve the results
-- as JSON, or fail if coordinates could not be read.
twoPointHandler :: (FromRow a, ToJSON r) =>
                   Query
                -> ([a] -> r)
                -- ^ Converts SQL results to a value served in JSON.
                -> Handler b (Geo b) ()
twoPointHandler q queryToResult = do
  c1 <- getCoordsParam "coords1"
  c2 <- getCoordsParam "coords2"

  case (c1, c2) of
    (Just (lon1, lat1), Just (lon2, lat2)) -> do
                   results <- fmap queryToResult $
                              withLens postgres $
                              query q (lon1, lat1, lon2, lat2)
                   modifyResponse $ setContentType "application/json"
                   writeLBS $ A.encode results
    _ -> error "Bad request"


------------------------------------------------------------------------------
-- | Query to fetch partners within a box, with mobile partners coming
-- last. See 'withinPartners' and `geowithin` SQL stored procedure.
--
-- Splice 14 parameters, starting with lon1, lat1 and lon2, lat2 on
-- the query, where coordinates are those of opposite 2D box points.
withinQuery :: Query
withinQuery = [sql|
SELECT geowithin(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
withinPartners :: Handler b (Geo b) ()
withinPartners = do
  c1 <- getCoordsParam "coords1"
  c2 <- getCoordsParam "coords2"
  c <- getCoordsParam "from"

  city <- fromMaybe ""  <$> getParam "city[]"
  make <- fromMaybe ""  <$> getParam "make[]"
  srv  <- fromMaybe ""  <$> getParam "services[]"
  sub  <- parseMayParam decimal "subtype"
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
                      :* srv  :* (sub :: Maybe Int)
                      :* pr2  :* pr3
                      :* dlr  :* mp
        results <- recode <$> withLens postgres (query withinQuery qParams)
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
distance :: Handler b (Geo b) ()
distance = twoPointHandler distanceQuery (head . head :: [[Double]] -> Double)


------------------------------------------------------------------------------
-- | True only for names of Russian cities which are federal subjects
-- (in UTF-8, ru-RU).
isFederal :: Text -> Bool
isFederal = (`elem` ["Москва", "Санкт-Петербург", "Севастополь"])


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
    toJSON (FullAddress c s) = object [ "city"    .= c
                                      , "address" .= s
                                      ]


------------------------------------------------------------------------------
-- | Use Nominatim ("mediator", see `carma-nominatim-mediator` package)
-- to perform a reverse search for an address at coordinates provided
-- in @coords@ request parameter. Response body is a JSON object
-- with keys @city@ and @address@ (possibly with `null` values),
-- or a single key @error@ if Nominatim geocoding failed completely.
revSearch :: Handler b (Geo b) ()
revSearch = do
  (!lon, !lat) <- fromMaybe (error "Bad request") <$> getCoordsParam "coords"
  lang         <- gets nominatimLang

  let urlpath = BSL.unpack
              $ BS.toLazyByteString
              $ HttpT.encodePathSegments
                ["reverse-search", lang, [qm| {lon},{lat} |]]

  req <- gets nominatimReq <*> pure urlpath >>= liftIO

  -- Read coords and send reverse geocoding request to Nominatim

  addr' <- liftIO
         $ fmap (eitherDecode' . HttpC.responseBody)
         $ HttpC.newManager HttpC.tlsManagerSettings >>= HttpC.httpLbs req

  -- Repack Nominatim response into a nicer JSON
  case addr' of
    Right addr -> writeJSON (addr :: FullAddress)
    Left  msg  -> do
      modifyResponse $ setResponseCode 500
      writeJSON $ M.singleton ("error" :: String) msg


-- | Use Nominatim ("mediator", see `carma-nominatim-mediator` package)
-- to perform a search for objects at an address provided
-- in @query@ request parameter. Response body is identical
-- to Nominatim's original response.
search :: Handler b (Geo b) ()
search = do
  !q   <- fromMaybe (error "Empty query") <$> getParamT "query"
  lang <- gets nominatimLang

  let urlpath = BSL.unpack
              $ BS.toLazyByteString
              $ HttpT.encodePathSegments ["search", lang, q]

  req <- liftIO =<< gets nominatimReq <*> pure urlpath

  res <- liftIO
       $ fmap HttpC.responseBody
       $ HttpC.newManager HttpC.tlsManagerSettings >>= HttpC.httpLbs req

  modifyResponse $ setContentType "application/json"
  writeLBS res


geoInit :: SnapletLens b Postgres -> SnapletInit b (Geo b)
geoInit db = makeSnaplet "geo" "Geoservices" Nothing $ do
  cfg <- getSnapletUserConfig
  lang <- liftIO $ lookupDefault "ru-RU,ru" cfg "nominatim-lang"

  nomReq <- liftIO $
    require cfg "nominatim-mediator-url"
      <&> -- Removing trailing slash if it is there,
          -- `HttpT.encodePathSegments` adds it.
          (\x -> if "/" `isSuffixOf` x then init x else x)
      <&> \url pathSfx -> HttpC.parseUrlThrow (url <> pathSfx)

  addRoutes routes
  pure $ Geo db nomReq lang
