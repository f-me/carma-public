{-# LANGUAGE FlexibleInstances #-}
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
import           Data.Aeson.Types (emptyArray)
import qualified Data.HashMap.Strict as HM
import           Data.Scientific (toRealFloat, toBoundedInteger)

import           Data.Attoparsec.ByteString.Char8

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Dict.New
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T (concat, pack, unpack)

import           Data.Configurator

import           Data.Time.Clock

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM (unsafeNew, unsafeWrite)

import qualified Network.HTTP as H

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple

import           Carma.Model.Types       (Coords(..))
import           Carma.Model.LegacyTypes (PickerField(..), Phone(..))
import           Data.Model              as Model
import qualified Data.Model.Patch        as Patch
import qualified Carma.Model.Case        as Case
import qualified Carma.Model.CaseStatus  as CS
import qualified Carma.Model.City        as City
import qualified Carma.Model.Partner     as Partner
import qualified Carma.Model.Program     as Program
import qualified Carma.Model.SubProgram  as SubProgram

import           Carma.HTTP.Base hiding (runCarma)
import qualified Carma.HTTP.Base as CH (runCarma)
import           Carma.HTTP.New


data GeoApp = GeoApp
    { _postgres :: Snaplet Postgres
    , carmaOptions :: CarmaOptions
    -- ^ Options of CaRMa running on localhost.
    , cityDict :: NewDict
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
           -> Handler b GeoApp (Maybe Text, Maybe Text)
revGeocode lon lat = do
  uri <- runCarma $ methodURI ("geo/revSearch/" ++ coordsToString lon lat)
  addr <- liftIO $ do
     resp <- H.simpleHTTP $ H.getRequest uri
     body <- H.getResponseBody resp
     return $ decode' $ BSL.pack body
  case addr :: Maybe (HM.HashMap Text (Maybe Text)) of
    Just m ->
        return ( fromMaybe Nothing (HM.lookup "city" m)
               , fromMaybe Nothing (HM.lookup "address" m))
    Nothing -> return (Nothing, Nothing)


------------------------------------------------------------------------------
-- | Update partner position from request parameters @lon@ and @lat@,
-- setting new address if possible.
updatePosition :: Handler b GeoApp ()
updatePosition = do
  lon' <- getParamWith double "lon"
  lat' <- getParamWith double "lat"
  free <- getParamWith bool "isFree"
  pid' <- getParamWith decimal "pid"
  case (lon', lat', pid') of
    (Just lon, Just lat, Just pid) -> do
       addr <- snd <$> revGeocode lon lat
       mtime <- liftIO $ getCurrentTime
       updatePartnerData (Ident pid) lon lat (fromMaybe True free) addr mtime
    _ -> error "Bad request"


------------------------------------------------------------------------------
-- | Send HTTP PUT request to CaRMa API to update partner data,
-- setting new values for fields @coords@, @isFree@, @addrs/fact@ and
-- @mtime@. Note that city value is not updated.
updatePartnerData :: IdentI Partner.Partner
                  -- ^ Partner id.
                  -> Double
                  -- ^ Longitude.
                  -> Double
                  -- ^ Latitude.
                  -> Bool
                  -- ^ Partner status.
                  -> (Maybe Text)
                  -- ^ New address if available (unchanged otherwise).
                  -> UTCTime
                  -- ^ New partner mtime.
                  -> Handler b GeoApp ()
updatePartnerData pid lon lat free addr mtime =
    let
        body = Patch.put Partner.coords (Just $ Coords (lon, lat)) $
               Patch.put Partner.isFree free $
               Patch.put Partner.mtime mtime $
               Patch.empty
    in do
      -- Update addrs with new "fact" address. Not thread-safe.
      body' <- case addr of
        Nothing -> return body
        Just newFactAddr -> do
          pData <- runCarma $ readInstance pid
          let oldAddrs = fromMaybe emptyArray $
                         Patch.get pData Partner.addrs
              newAddrs = setKeyedJsonValue oldAddrs "fact" (String newFactAddr)
          return $ Patch.put Partner.addrs newAddrs body
      runCarma $ updateInstance pid body' >> return ()


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
  let -- Do not enforce typing on values when reading JSON
      Just (Object jsonRq0) = Aeson.decode rqb
      coords' = (,) <$> (HM.lookup "lon" jsonRq0) <*> (HM.lookup "lat" jsonRq0)
      isAccident  = HM.lookup "isAccident" jsonRq0
      program'    = HM.lookup (fieldName Case.program) jsonRq0
      subprogram' = HM.lookup (fieldName Case.subprogram) jsonRq0

      -- Now read all values but coords and program as Texts
      jsonRq = HM.fromList
               [(k, s) | (k, String s) <- HM.toList jsonRq0]
      car_make = HM.lookup "car_make" jsonRq
      -- Add a text field from the request to a patch
      putFromRequest acc =
        Patch.put acc (HM.lookup (fieldName acc) jsonRq)

  carMakeId
    <- (\case { [[makeId]] -> Just makeId; _ -> Nothing })
    <$> query [sql|select id from "CarMake" where value = ?|] [car_make]

  -- Start building a JSON for CaRMa
  let caseBody = Patch.put Case.car_make carMakeId $
                 putFromRequest Case.car_plateNum $
                 putFromRequest Case.car_vin $
                 Patch.put Case.contact_phone1
                 (Phone <$> HM.lookup (fieldName Case.contact_phone1) jsonRq) $
                 putFromRequest Case.contact_name $
                 putFromRequest Case.contact_email $
                 Patch.put Case.contractIdentifier
                 (HM.lookup "cardNumber_cardNumber" jsonRq) $
                 Patch.empty

  dict <- gets cityDict

  caseBody' <- case coords' of
    -- Reverse geocode coordinates from lon/lat
    Just (Number lon', Number lat') ->
      let (lon, lat) = (toRealFloat lon', toRealFloat lat')
      in revGeocode lon lat >>= \case
        (city, addr) ->
          return $
          -- Translate input city name to corresponding dictionary key
          maybe id (Patch.put Case.city . Just . Ident)
          (city >>= (flip valueOfLabel dict)) $
          -- Prepend street address with city name
          maybe id
          (\a ->
             Patch.put Case.caseAddress_address $
             (PickerField $ Just $
              (maybe a (\c -> T.concat [c, ", ", a]) city))) addr $
          (Patch.put Case.caseAddress_coords $
           (PickerField $ Just $ T.pack $ coordsToString lon lat)) $
          caseBody
    _ -> return caseBody

  let numberToIdent :: Maybe Value -> Maybe (IdentI n)
      numberToIdent (Just (Number n)) = Ident <$> toBoundedInteger n
      numberToIdent _                 = Nothing

      -- Set default program/subprogram (if not provided by client)
      (progValue, subProgValue) =
          case (program', subprogram') of
            (Just (Aeson.String "Cadillac"), _) ->
                (Just Program.gm, Just SubProgram.cad2012)
            (a, b) -> (numberToIdent a, numberToIdent b)

      -- TODO Check that subprogram.parent = program
      caseBody'' = Patch.put Case.program
                   (fromMaybe Program.ramc progValue) $
                   Patch.put Case.subprogram
                   (Just
                    (fromMaybe SubProgram.ramc subProgValue)) $
                   caseBody'

  caseId <- runCarma $ do
    (caseId, _) <- createInstance caseBody''
    -- Trigger an avalanche
    let newStatus = case isAccident of
                      Just (Aeson.Bool True) -> CS.mobileAccident
                      _                      -> CS.mobileOrder
    updateInstance caseId (Patch.put Case.caseStatus newStatus Patch.empty)
    return caseId

  modifyResponse $ setContentType "application/json"
  writeLBS . encode $ object $ [ "caseId" .= caseId ]


------------------------------------------------------------------------------
-- | Wrapper type for @/geo/partnersAround@ results.
newtype Partner = Partner (Int, Double, Double, Maybe Bool, Maybe Bool,
                           Maybe Text, Maybe Text, Maybe Text)


instance FromRow Partner where
  fromRow = Partner <$> fromRow


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
  SELECT DISTINCT ON (partnertbl.id) partnertbl.*,
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
  dist  <- getParamWith decimal "dist"
  case cds of
    Just (lon, lat) -> do
      let qParams = ( lon
                    , lat
                    , isNothing brand
                    , fromMaybe "" brand
                    , isNothing dist
                    , maybe 100000 (* 1000) dist :: Int
                    , fromMaybe "20" limit
                    )
      results <- query partnersAroundQuery qParams
      modifyResponse $ setContentType "application/json"
      writeLBS $ Aeson.encode (results :: [Partner])
    _ -> error "Bad request"


geoAppInit :: SnapletInit b GeoApp
geoAppInit = makeSnaplet "geo" "Geoservices" Nothing $ do
    db <- nestSnaplet "postgres" postgres pgsInit
    cfg <- getSnapletUserConfig

    cp <- liftIO $ lookupDefault 8000 cfg "carma_port"
    let cOpts = defaultCarmaOptions{carmaPort = cp}
        defDict = T.unpack $ modelName (modelInfo :: ModelInfo City.City)
    dName <- liftIO $ lookupDefault defDict cfg "cities-dictionary"

    addRoutes routes
    cDict' <- liftIO $ CH.runCarma cOpts $ readDictionary dName
    case cDict' of
      Just cDict -> return $ GeoApp db cOpts (loadNewDict' cDict)
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
