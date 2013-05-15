{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-|

Standalone application used to update partner coordinates and create
new cases in CaRMa. Used as a gateway to CaRMa for mobile
applications.

-}

module Application
    ( GeoApp
    , geoAppInit)

where

import Control.Applicative
import Control.Lens hiding ((.=), createInstance)
import Control.Monad
import Control.Monad.State hiding (ap)

import Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM

import Data.Attoparsec.ByteString.Char8

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Dict
import Data.Maybe
import qualified Data.Text as T (pack)
import Data.Text.Encoding (encodeUtf8)

import Data.Configurator

import Data.Time.Clock
import Data.Time.Format

import qualified Database.Redis as R
import Database.PostgreSQL.Simple.SqlQQ

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM (unsafeNew, unsafeWrite)

import qualified Network.HTTP as H

import System.Locale

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.RedisDB


import Carma.HTTP


data GeoApp = GeoApp
    { _postgres :: Snaplet Postgres
    , _redis :: Snaplet RedisDB
    , carmaPort :: Int
    -- ^ Port of CaRMa instance running on localhost.
    , cityDict :: Dict
    -- ^ Dictionary used to map city names to internal values.
    }

makeLenses ''GeoApp


instance HasPostgres (Handler b GeoApp) where
    getPostgresState = with postgres $ get


routes :: [(ByteString, Handler b GeoApp ())]
routes = [ ("/geo/partner/:pid", method PUT $ updatePosition)
         , ("/geo/partner/:pid", method GET $ getMessage)
         , ("/geo/partnersAround/:coords", method GET $ partnersAround)
         , ("/geo/case/", method POST $ newCase)
         ]


getCarmaPort :: Handler b GeoApp Int
getCarmaPort = gets carmaPort


------------------------------------------------------------------------------
-- | Pack coordinates to a string of format @33.12,57.32@.
coordsToString :: Double -> Double -> String
coordsToString lon lat = (show lon) ++ "," ++ (show lat)


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
-- | Try to obtain city and street address from coordinates using
-- reverse geocoding.
revGeocode :: Double
           -- ^ Longitude.
           -> Double
           -- ^ Latitude.
           -> Handler b GeoApp (Maybe ByteString, Maybe ByteString)
revGeocode lon lat = do
  cp <- getCarmaPort
  (addr :: Maybe (HM.HashMap ByteString (Maybe ByteString))) <- liftIO $ do
     let coords = coordsToString lon lat
     resp <- H.simpleHTTP $ H.getRequest $
             methodURI cp ("geo/revSearch/" ++ coords)
     body <- H.getResponseBody resp
     return $ decode' $ BSL.pack body
  case addr of
    Just m ->
        return ( fromMaybe Nothing $ HM.lookup "city" m
               , fromMaybe Nothing $ HM.lookup "address" m)
    Nothing -> return (Nothing, Nothing)


------------------------------------------------------------------------------
-- | Update partner position from request parameters @lon@ and @lat@,
-- setting new address if possible.
updatePosition :: Handler b GeoApp ()
updatePosition = do
  lon' <- getParamWith double "lon"
  lat' <- getParamWith double "lat"
  (pid' :: Maybe Int) <- getParamWith decimal "pid"
  case (lon', lat', pid') of
    (Just lon, Just lat, Just pid) -> do
       addr <- snd <$> revGeocode lon lat
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
                  -> (Maybe ByteString)
                  -- ^ New address if available.
                  -> UTCTime
                  -- ^ New partner mtime.
                  -> Handler b GeoApp ()
updatePartnerData pid lon lat addr mtime =
    let
        coordString = concat [show lon, ",", show lat]
        body = HM.fromList $
               [ (partnerCoords, BS.pack coordString)
               , (partnerMtime,
                  BS.pack $ formatTime defaultTimeLocale "%s" mtime)] ++
               (maybe [] (\a -> [(partnerAddress, a)]) addr)
    in do
      cp <- getCarmaPort
      liftIO $ updateInstance cp "partner" pid body >> return ()


------------------------------------------------------------------------------
-- | Used to fetch messages for a partner with id supplied as a query
-- parameter.
getMessageQuery :: Query
getMessageQuery = [sql|
SELECT message FROM partnerMessageTbl
WHERE partnerId=? order by ctime desc limit 1;
|]


getMessage :: Handler b GeoApp ()
getMessage = do
  Just pid <- getParam "pid"
  let partnerId = BS.append "partner:" pid
  res <- query getMessageQuery $ Only partnerId
  case res of
    (Only msg):_ -> writeLBS msg
    _ -> writeLBS "{}"


------------------------------------------------------------------------------
-- | Name of case coordinates field in case model.
caseCoords :: ByteString
caseCoords = "caseAddress_coords"


------------------------------------------------------------------------------
-- | Name of case address field in case model.
caseAddress :: ByteString
caseAddress = "caseAddress_address"


------------------------------------------------------------------------------
-- | Name of case city field in case model.
caseCity :: ByteString
caseCity = "city"


caseProgram :: ByteString
caseProgram = "program"


defaultProgram :: ByteString
defaultProgram = "ramc2"


------------------------------------------------------------------------------
-- | Name of case id field in action model.
actionCaseId :: ByteString
actionCaseId = "caseId"


------------------------------------------------------------------------------
-- | Name of actions field in case model.
caseActions :: ByteString
caseActions = "actions"


------------------------------------------------------------------------------
-- | Build reference to a case for use in 'actionCaseId'.
caseIdReference :: Int -> ByteString
caseIdReference n = BS.pack $ "case:" ++ (show n)


------------------------------------------------------------------------------
-- | Build reference to an action for use in 'caseActions'.
actionIdReference :: Int -> ByteString
actionIdReference n = BS.pack $ "action:" ++ (show n)


------------------------------------------------------------------------------
-- | Create a new case from a JSON object provided in request body.
-- Perform reverse geocoding using coordinates from values under @lon@
-- and @lat@ (read as JSON doubles), writing the obtained city and
-- street address to the new case. New @callMeMaybe@ action is created
-- for the case.
--
-- Response body is a JSON of form @{"caseId":<n>}@, where @n@ is the
-- new case id.
newCase :: Handler b GeoApp ()
newCase = do
  -- New case parameters
  rqb <- readRequestBody 4096
  let -- Do not enforce typing on values when reading JSON
      Just jsonRq0 :: Maybe (HM.HashMap ByteString Value) =
                      Aeson.decode rqb
      coords = (,) <$> (HM.lookup "lon" jsonRq0) <*> (HM.lookup "lat" jsonRq0)
      -- Now read all values but coords into ByteStrings
      jsonRq = HM.map (\(String s) -> encodeUtf8 s) $
               HM.filter (\case
                          String _ -> True
                          _        -> False)
               jsonRq0
      car_vin = HM.lookup "car_vin" jsonRq

  dict <- gets cityDict

  jsonRq' <- case coords of
    -- Reverse geocode coordinates from lon/lat
    Just (Number (D lon), Number (D lat)) -> revGeocode lon lat >>= \case
      (city, addr) ->
        return $
        -- Translate input city name to corresponding dictionary key
        (maybe id (HM.insert caseCity) $ city >>= (flip valueOfLabel dict)) $
        -- Prepend street address with city name
        (maybe id (\a -> HM.insert caseAddress
                         (maybe a (\c -> BS.concat [c, ", ", a]) city))
                         addr) $
        HM.insert caseCoords (BS.pack $ coordsToString lon lat) $
        jsonRq
    _ -> return jsonRq



  -- Set default program (if not provided by client), then form a new
  -- case request to send to CaRMa
  let setProg   = maybe id (const $ HM.insert caseProgram defaultProgram)
                  (HM.lookup "program" jsonRq')
      caseBody  = setProg $
                  HM.delete "lon" $
                  HM.delete "lat" $
                  HM.delete "car_vin" $ -- we'll insert it later to run trigger
                  jsonRq'

  modifyResponse $ setContentType "application/json"
  cp <- getCarmaPort
  caseResp <- liftIO $ createInstance cp "case" caseBody

  now <- liftIO $ getCurrentTime
  let nowStr = BS.pack $ formatTime defaultTimeLocale "%s" (now :: UTCTime)
  -- Fetch id of the created case and create a new action for this id
  let caseId = fst caseResp
      descr = encodeUtf8 $ T.pack
            $  "Клиент заказал услугу с помощью мобильного приложения. "
            ++ "Требуется перезвонить ему и уточнить детали"
      actBody = HM.fromList
                [ (actionCaseId, caseIdReference caseId)
                , ("name", "callMeMaybe")
                , ("targetGroup", "back")
                , ("duetime", nowStr)
                , ("priority", "1")
                , ("closed", "0")
                , ("description", descr)
                ]
  actResp <- liftIO $ createInstance cp "action" actBody

  -- Fetch id of the created action and update the reverse reference
  -- in the case.
  let actId = fst actResp
  liftIO $ updateInstance cp "case" caseId $ HM.fromList $
          [ (caseActions, actionIdReference actId) ]
          -- we update car_vin here to trigger vin-search (it's a bit
          -- easier than adding correct trigger handling on POST
          -- request)
          ++ maybe [] (\vin -> [("car_vin", vin)]) car_vin

  writeLBS . encode $ object $ [ "caseId" .= caseId ]


------------------------------------------------------------------------------
-- | Wrapper type for @/geo/partnersAround@ results.
newtype Partner = Partner (Int, Double, Double, Maybe Bool, Maybe Bool,
                           Maybe ByteString, Maybe ByteString, Maybe ByteString)
                  deriving (FromRow)


instance ToJSON Partner where
  toJSON (Partner (a, b, c, d, e, f, g, h)) = Array $ V.create $ do
    mv <- VM.unsafeNew 8
    VM.unsafeWrite mv 0 (toJSON a)
    VM.unsafeWrite mv 1 (toJSON b)
    VM.unsafeWrite mv 2 (toJSON c)
    VM.unsafeWrite mv 3 (toJSON d)
    VM.unsafeWrite mv 4 (toJSON e)
    VM.unsafeWrite mv 5 (toJSON f)
    VM.unsafeWrite mv 6 (toJSON g)
    VM.unsafeWrite mv 7 (toJSON h)
    return mv
  {-# INLINE toJSON #-}


------------------------------------------------------------------------------
-- | Splice 5 parameters: car brand (boolean flag if this filter is
-- enabled (active low) & value for this parameter), lon, lat and
-- limit onto query; then fetch list of partners ordered by distance
-- to the provided point, in ascending order. Serve only dealer
-- partners with non-empty names.
partnersAroundQuery :: Query
partnersAroundQuery = [sql|
SELECT id, st_x(coords), st_y(coords),
isDealer, isMobile,
name, addrDeFacto, phone1
FROM partnertbl
WHERE isDealer
AND name != ''
AND (? or ? = ANY(makes))
ORDER BY
ST_Distance_Sphere(coords, ST_PointFromText('POINT(? ?)', 4326)) ASC
LIMIT ?;
|]


------------------------------------------------------------------------------
-- | Read @coords@, @limit@ parameters and server JSON list of
-- partners around a point.
partnersAround :: Handler b GeoApp ()
partnersAround = do
  cds <- getParamWith coords "coords"
  limit <- getParam "limit"
  brand <- getParam "car_make"
  case cds of
    Just (lon, lat) -> do
      let qParams = ( isNothing brand
                    , fromMaybe "" brand
                    , lon
                    , lat
                    , fromMaybe "20" limit
                    )
      results :: [Partner] <- query partnersAroundQuery qParams
      modifyResponse $ setContentType "application/json"
      writeLBS $ Aeson.encode results
    _ -> error "Bad request"


geoAppInit :: SnapletInit b GeoApp
geoAppInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    rdb <- nestSnaplet "redis" redis $ redisDBInit R.defaultConnectInfo
    cfg <- getSnapletUserConfig

    cp <- liftIO $ lookupDefault 8000 cfg "carma_port"
    dName <- liftIO $ lookupDefault "DealerCities" cfg "cities-dictionary"

    addRoutes routes
    cDict' <- liftIO $ readDictionary cp dName
    case cDict' of
      Just cDict -> return $ GeoApp db rdb cp cDict
      Nothing -> error "Could not load cities dictionary from CaRMa"


------------------------------------------------------------------------------
-- | Parse "52.32,3.45" (no spaces) into pair of doubles.
coords :: Parser (Double, Double)
coords = (,) <$> double <* anyChar <*> double


------------------------------------------------------------------------------
-- | Apply a parser to read data from a named request parameter.
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
