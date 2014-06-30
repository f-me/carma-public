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

import           Control.Applicative
import           Control.Lens hiding ((.=), createInstance)
import           Control.Monad
import           Control.Monad.State hiding (ap)

import           Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM

import           Data.Attoparsec.Number
import           Data.Attoparsec.ByteString.Char8

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Dict
import           Data.Maybe
import qualified Data.Text as T (pack)
import           Data.Text.Encoding (encodeUtf8)

import           Data.Configurator

import           Data.Time.Clock
import           Data.Time.Format

import           Database.PostgreSQL.Simple.SqlQQ

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM (unsafeNew, unsafeWrite)

import qualified Network.HTTP as H

import           System.Locale

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple

import           Data.Model             as Model
import qualified Carma.Model.Case       as Case
import qualified Carma.Model.Program    as Program
import qualified Carma.Model.SubProgram as SubProgram
import qualified Carma.Model.Role       as Role

import           Carma.HTTP hiding (runCarma)
import qualified Carma.HTTP as CH (runCarma)


data GeoApp = GeoApp
    { _postgres :: Snaplet Postgres
    , carmaOptions :: CarmaOptions
    -- ^ Options of CaRMa running on localhost.
    , cityDict :: Dict
    -- ^ Dictionary used to map city names to internal values.
    }

makeLenses ''GeoApp


instance HasPostgres (Handler b GeoApp) where
    getPostgresState = with postgres $ get


routes :: [(ByteString, Handler b GeoApp ())]
routes = [ ("/geo/partner/:pid",           method PUT $ updatePosition)
         , ("/geo/partnersAround/:coords", method GET $ partnersAround)
         , ("/geo/case/",                  method POST $ newCase)
         ]


runCarma :: CarmaIO a -> Handler b GeoApp a
runCarma action = do
  co <- gets carmaOptions
  liftIO $ CH.runCarma co action


------------------------------------------------------------------------------
-- | Pack coordinates to a string of format @33.12,57.32@.
coordsToString :: Double -> Double -> String
coordsToString lon lat = (show lon) ++ "," ++ (show lat)


------------------------------------------------------------------------------
-- | Try to obtain city and street address from coordinates using
-- reverse geocoding.
revGeocode :: Double
           -- ^ Longitude.
           -> Double
           -- ^ Latitude.
           -> Handler b GeoApp (Maybe ByteString, Maybe ByteString)
revGeocode lon lat = do
  uri <- runCarma $ methodURI ("geo/revSearch/" ++ coordsToString lon lat)
  (addr :: Maybe (HM.HashMap ByteString (Maybe ByteString))) <- liftIO $ do
     resp <- H.simpleHTTP $ H.getRequest uri
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
  free <- getParamWith bool "isFree"
  (pid' :: Maybe Int) <- getParamWith decimal "pid"
  case (lon', lat', pid') of
    (Just lon, Just lat, Just pid) -> do
       addr <- snd <$> revGeocode lon lat
       mtime <- liftIO $ getCurrentTime
       updatePartnerData pid lon lat (fromMaybe True free) addr mtime
    _ -> error "Bad request"


------------------------------------------------------------------------------
-- | Send HTTP PUT request to CaRMa API to update partner data,
-- setting new values for fields @coords@, @isFree@, @addrs/fact@ and
-- @mtime@. Note that city value is not updated.
updatePartnerData :: Int
                  -- ^ Partner id.
                  -> Double
                  -- ^ Longitude.
                  -> Double
                  -- ^ Latitude.
                  -> Bool
                  -- ^ Partner status.
                  -> (Maybe ByteString)
                  -- ^ New address if available.
                  -> UTCTime
                  -- ^ New partner mtime.
                  -> Handler b GeoApp ()
updatePartnerData pid lon lat free addr mtime =
    let
        -- Name of address field in partner model (must contain JSON
        -- data with dict-objects schema).
        partnerAddress = "addrs"
        coordString = concat [show lon, ",", show lat]
        isFree = if free then "1" else "0"
        body = HM.fromList $
               [ ("coords", BS.pack coordString)
               , ("isFree", BS.pack isFree)
               , ("mtime",
                  BS.pack $ formatTime defaultTimeLocale "%s" mtime)] ++
               (maybe [] (\a -> [(partnerAddress, a)]) addr)
    in do
      -- Update addrs with new "fact" address. Not thread-safe.
      body' <- case addr of
        Nothing -> return body
        Just newFactAddr -> do
          pData <- runCarma $ readInstance "partner" pid
          let oldAddrs = fromMaybe "" $ HM.lookup partnerAddress pData
              newAddrs = setKeyedJsonValue oldAddrs (Right "fact") newFactAddr
          return $ HM.insert partnerAddress newAddrs body
      runCarma $ updateInstance "partner" pid body' >> return ()


------------------------------------------------------------------------------
-- | Build a reference to a case.
caseIdReference :: Int -> ByteString
caseIdReference n = BS.pack $ "case:" ++ (show n)


------------------------------------------------------------------------------
-- | Build a reference to an action for use in @actions@ of case.
actionIdReference :: Int -> ByteString
actionIdReference n = BS.pack $ "action:" ++ (show n)


roleIdent :: IdentI Role.Role -> ByteString
roleIdent (Ident v) = BS.pack $ show v


------------------------------------------------------------------------------
-- | Create a new case from a JSON object provided in request body.
-- Perform reverse geocoding using coordinates from values under @lon@
-- and @lat@ (read as JSON doubles), writing the obtained city and
-- street address to the new case. New @callMeMaybe@ action is created
-- for the case. @program@ parameter is read as a number, but
-- @Cadillac@ string value is also supported.
--
-- Response body is a JSON of form @{"caseId":<n>}@, where @n@ is the
-- new case id.
newCase :: Handler b GeoApp ()
newCase = do
  -- New case parameters
  rqb <- readRequestBody 4096
  let progField    = encodeUtf8 $ fieldName Case.program
      subProgField = encodeUtf8 $ fieldName Case.subprogram
      -- Do not enforce typing on values when reading JSON
      Just jsonRq0 :: Maybe (HM.HashMap ByteString Value) =
                      Aeson.decode rqb
      coords' = (,) <$> (HM.lookup "lon" jsonRq0) <*> (HM.lookup "lat" jsonRq0)
      program'    = HM.lookup progField jsonRq0
      subprogram' = HM.lookup subProgField jsonRq0
      -- Now read all values but coords and program into ByteStrings
      jsonRq = HM.map (\(String s) -> encodeUtf8 s) $
               HM.filter (\case
                          String _ -> True
                          _        -> False)
               jsonRq0
      car_vin = HM.lookup "car_vin" jsonRq
      car_make = HM.lookup "car_make" jsonRq

  carMakeId
    <- (\case { [[makeId]] -> Just makeId; _ -> Nothing })
    <$> query [sql|select id::text from "CarMake" where value = ?|] [car_make]

  dict <- gets cityDict

  jsonRq' <- case coords' of
    -- Reverse geocode coordinates from lon/lat
    Just (Number (D lon), Number (D lat)) -> revGeocode lon lat >>= \case
      (city, addr) ->
        return $
        -- Translate input city name to corresponding dictionary key
        (maybe id (HM.insert "city") $ city >>= (flip valueOfLabel dict)) $
        -- Prepend street address with city name
        (maybe id (\a -> HM.insert "caseAddress_address"
                         (maybe a (\c -> BS.concat [c, ", ", a]) city))
                         addr) $
        HM.insert "caseAddress_coords" (BS.pack $ coordsToString lon lat) $
        jsonRq
    _ -> return jsonRq

  let numberToInt :: Maybe Value -> Maybe Int
      numberToInt (Just (Number (I n))) = Just $ fromIntegral n
      numberToInt _                     = Nothing

      identToInt :: IdentI m -> Int
      identToInt i = n where (Ident n) = i

      -- Set default program/subprogram (if not provided by client)
      (progValue, subProgValue) =
          case (program', subprogram') of
            (Just (Aeson.String "Cadillac"), _) ->
                (Just $ identToInt Program.gm,
                 Just $ identToInt SubProgram.cad2012)
            (a, b) -> (numberToInt a, numberToInt b)

      -- Form a new case request to send to CaRMa
      caseBody  = HM.insert progField
                  (BS.pack $ show $
                   fromMaybe (identToInt Program.ramc) progValue) $
                  HM.insert subProgField
                  (BS.pack $ show $
                   fromMaybe (identToInt SubProgram.ramc) subProgValue) $
                  HM.delete "lon" $
                  HM.delete "lat" $
                  HM.delete "car_vin" $ -- We insert it later to run the trigger
                  maybe id (HM.insert "car_make") carMakeId $
                  jsonRq'

  modifyResponse $ setContentType "application/json"
  caseResp <- runCarma $ createInstance "case" caseBody

  now <- liftIO $ getCurrentTime
  let nowStr = BS.pack $ formatTime defaultTimeLocale "%s" (now :: UTCTime)
  -- Fetch id of the created case and create a new action for this id
  let caseId = fst caseResp
      descr = encodeUtf8 $ T.pack
            $  "Клиент заказал услугу с помощью мобильного приложения. "
            ++ "Требуется перезвонить ему и уточнить детали"
      actBody = HM.fromList
                [ ("caseId", caseIdReference caseId)
                , ("name", "callMeMaybe")
                , ("targetGroup", roleIdent Role.bo_order)
                , ("duetime", nowStr)
                , ("priority", "1")
                , ("closed", "0")
                , ("description", descr)
                ]
  actResp <- runCarma $ createInstance "action" actBody

  -- Fetch id of the created action and update the reverse reference
  -- in the case.
  let actId = fst actResp
  runCarma $ updateInstance "case" caseId $ HM.fromList $
          [ ("actions", actionIdReference actId) ]
          -- We update contractIdentifier here to trigger contract search
          ++ maybe [] (\vin -> [("contractIdentifier", vin)]) car_vin

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
-- | Splice 5 parameters: lon, lat, car brand (boolean flag if this
-- filter is enabled (active low) & value for this parameter), maximum
-- distance (flag & value) and limit onto query; then fetch list of
-- partners ordered by distance to the provided point, in ascending
-- order. Serve only dealer partners with non-empty names. First
-- @fact@ address and first @disp@ phone are included for each
-- partner.
partnersAroundQuery :: Query
partnersAroundQuery = [sql|
WITH subquery AS (
  SELECT partnertbl.*,
         addr->>'value' as addrDeFacto,
         phone->>'value' as phone1,
         ST_Distance_Sphere(coords, ST_PointFromText('POINT(? ?)', 4326)) dist
  FROM partnertbl,
       json_array_elements(addrs) as addr,
       json_array_elements(phones) as phone,
       unnest(makes) as partner_make,
       "CarMake"
  WHERE isDealer
  AND addr->>'key' = 'fact'
  AND phone->>'key' = 'disp'
  AND name != ''
  AND "CarMake".id = partner_make
  AND (? or ? = "CarMake".value))
SELECT id, st_x(coords), st_y(coords),
isDealer, isMobile,
name, addrDeFacto, phone1
FROM subquery
WHERE (? or dist < ?)
ORDER BY dist ASC
LIMIT ?;
|]


------------------------------------------------------------------------------
-- | Read @coords@, @limit@, @car_make@, @dist@ request parameters and
-- serve a JSON list of partners around a point.
partnersAround :: Handler b GeoApp ()
partnersAround = do
  cds <- getParamWith coords "coords"
  limit <- getParam "limit"
  brand <- getParam "car_make"
  (dist :: Maybe Int)  <- getParamWith decimal "dist"
  case cds of
    Just (lon, lat) -> do
      let qParams = ( lon
                    , lat
                    , isNothing brand
                    , fromMaybe "" brand
                    , isNothing dist
                    , maybe 100000 (* 1000) dist
                    , fromMaybe "20" limit
                    )
      results :: [Partner] <- query partnersAroundQuery qParams
      modifyResponse $ setContentType "application/json"
      writeLBS $ Aeson.encode results
    _ -> error "Bad request"


geoAppInit :: SnapletInit b GeoApp
geoAppInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    cfg <- getSnapletUserConfig

    cp <- liftIO $ lookupDefault 8000 cfg "carma_port"
    let cOpts = defaultCarmaOptions{carmaPort = cp}
    dName <- liftIO $ lookupDefault "DealerCities" cfg "cities-dictionary"

    addRoutes routes
    cDict' <- liftIO $ CH.runCarma cOpts $ readDictionary dName
    case cDict' of
      Just cDict -> return $ GeoApp db cOpts cDict
      Nothing -> error "Could not load cities dictionary from CaRMa"


------------------------------------------------------------------------------
-- | Parse "52.32,3.45" (no spaces) into pair of doubles.
coords :: Parser (Double, Double)
coords = (,) <$> double <* anyChar <*> double


------------------------------------------------------------------------------
-- | Parse "true" or "false" into boolean.
bool :: Parser Bool
bool = (string "true" >> return True) <|> (string "false" >> return False)


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
