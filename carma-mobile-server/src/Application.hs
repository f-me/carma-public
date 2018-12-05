{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

{-|

Standalone application used to update partner coordinates and create
new cases in CaRMa. Used as a gateway to CaRMa for mobile
applications.

-}

module Application
     ( GeoApp
     , geoAppInit
     ) where

import           Control.Applicative
import           Control.Lens hiding ((.=), (<&>))
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State hiding (ap)

import           Data.Semigroup ((<>))
import           Data.List (find)
import           Data.Aeson as Aeson
import           Data.Aeson.Types (emptyArray)
import qualified Data.HashMap.Strict as HM
import           Data.Scientific (toRealFloat, toBoundedInteger)

import           Data.Attoparsec.ByteString.Char8

import qualified Data.ByteString.Char8 as BS
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
import           Snap.Extras
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple

import           Carma.Utils.Operators
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


data SSLClientCertificate
   = SSLClientCertificate
   { knownCertificates :: [(ByteString, Text)]
   -- ^ @fst@ is a certificate file contents and @snd@ is a partner title
   }


data GeoApp = GeoApp
    { _postgres :: Snaplet Postgres
    , _sslClientCertificate :: Snaplet SSLClientCertificate
    , carmaOptions :: CarmaOptions
    -- ^ Options of CaRMa running on localhost.
    , cityDict :: NewDict
    -- ^ Dictionary used to map city names to internal values.
    }

makeLenses ''GeoApp


instance HasPostgres (Handler b GeoApp) where
    getPostgresState = with postgres get
    setLocalPostgresState s = local (set (postgres . snapletValue) s)


routes :: [(ByteString, Handler b GeoApp ())]
routes = [ ("/geo/partner/:pid",           method PUT updatePosition)
         , ("/geo/partnersAround/:coords", method GET partnersAround)
         , ("/geo/case/",                  method POST newCase)
         ]


runCarma :: CarmaIO a -> Handler b GeoApp a
runCarma action = do
  co <- gets carmaOptions
  liftIO $ CH.runCarma co action


------------------------------------------------------------------------------
-- | Pack coordinates to a string of format @33.12,57.32@.
coordsToString :: Double -> Double -> String
coordsToString lon lat = show lon ++ "," ++ show lat


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
  lon' <- parseMayParam double "lon"
  lat' <- parseMayParam double "lat"
  free <- parseMayParam bool "isFree"
  pid' <- parseMayParam decimal "pid"
  case (lon', lat', pid') of
    (Just lon, Just lat, Just pid) -> do
       addr <- snd <$> revGeocode lon lat
       mtime <- liftIO getCurrentTime
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
                  -> Maybe Text
                  -- ^ New address if available (unchanged otherwise).
                  -> UTCTime
                  -- ^ New partner mtime.
                  -> Handler b GeoApp ()
updatePartnerData pid lon lat free addr mtime =
    let
        body = Patch.put Partner.coords (Just $ Coords (lon, lat)) $
               Patch.put Partner.isFree free $
               Patch.put Partner.mtime mtime
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
      runCarma $ void (updateInstance pid body')



-- | A query to return a previously created case, 1 parameter (contact
-- phone)
oldCaseQ :: Query
oldCaseQ =
  [sql|
   SELECT id FROM casetbl
   WHERE contact_phone1 = ?
     AND callDate >= now() - interval '10 minutes'
   ORDER BY id DESC
   LIMIT 1;
   |]


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

  -- Obtaining custom header with @$ssl_client_cert@ value from
  -- nginx's @ssl_verify_client@ feature to authenticate external service
  -- by SSL client certificate and to add proper mark to @contact_name@
  -- to indicate where a @Case@ came from.
  --
  -- If it's @Nothing@ it means we're testing or using old legacy ports.
  --
  -- It's supposed to be forwarded like this (part of nginx config):
  --
  -- @
  -- location / {
  --   proxy_pass http://127.0.0.1:40443;
  --   proxy_set_header SSLClientCertificate $ssl_client_cert;
  -- }
  -- @
  sslClientCert <-
    getRequest
    <&> getHeader "SSLClientCertificate" -- Custom header (@proxy_set_header@)
    <&> fmap cleanSSLCertificate -- Map inside @Maybe@

  !partnerTitle <-
    case sslClientCert of
         Nothing ->
           -- SSL Client Certificate isn't provided,
           -- we're testing or using legacy old ports.
           pure Nothing

         Just cert ->
           -- SSL Client Certificate is provided,
           -- validating it and obtaining associated partner title.
           obtainPartnerTitle cert >>= \case
             Nothing -> do
               -- Such certificate not found in known client certificates list!
               modifyResponse $ setResponseStatus 406 "Not Acceptable"

               writeBS
                 "Not Acceptable! Your SSL client certificate not found\
                 \ in our known certificates list!"

               getResponse >>= finishWith

             Just partnerTitle ->
               -- Partner title is successfully obtained.
               pure $ Just partnerTitle

  let -- Do not enforce typing on values when reading JSON
      Just (Object jsonRq0) = Aeson.decode rqb
      coords' = (,) <$> HM.lookup "lon" jsonRq0 <*> HM.lookup "lat" jsonRq0
      isAccident  = HM.lookup "isAccident" jsonRq0
      program'    = HM.lookup (fieldName Case.program) jsonRq0
      subprogram' = HM.lookup (fieldName Case.subprogram) jsonRq0

      -- Now read all values but @coords@ and @program@ as @Text@s
      jsonRq =
        let
          l = HM.fromList [(k, s) | (k, String s) <- HM.toList jsonRq0]
          fieldKey = fieldName Case.contact_name
        in
          flip (maybe l) partnerTitle $ \p ->
            case HM.lookup fieldKey l of
                 Nothing ->
                   -- "contact_name" is not set, just adding it with only
                   -- partner's mark.
                   HM.insert fieldKey p l

                 Just v | v == p ->
                            -- Partner is already makred new @Case@ properly,
                            -- avoiding doubling a partner's mark.
                            l

                        | otherwise ->
                            -- "contact_name" is already set, just adding
                            -- partner's mark as suffix inside parenthesis.
                            HM.insert fieldKey (v <> " (" <> p <> ")") l

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
                 (HM.lookup "cardNumber_cardNumber" jsonRq)
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
          (city >>= (`valueOfLabel` dict)) $
          -- Prepend street address with city name
          maybe id
          (\a ->
             Patch.put Case.caseAddress_address
             (PickerField $ Just
              (maybe a (\c -> T.concat [c, ", ", a]) city))) addr $
          Patch.put Case.caseAddress_coords
          (PickerField $ Just $ T.pack $ coordsToString lon lat)
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
                    (fromMaybe SubProgram.ramc subProgValue))
                   caseBody'

  -- Check if there has been a recent case from this number. If so, do
  -- not create a new case but serve the old id.
  oldId <-
    case HM.lookup (fieldName Case.contact_phone1) jsonRq of
      Nothing -> return Nothing
      Just t ->
        query oldCaseQ [t] >>=
          \case
            [[i]] -> return $ Just i
            _     -> return Nothing

  caseId <- case oldId of
    Just oi -> return $ Ident oi
    Nothing -> runCarma $ do
      (caseId, _) <- createInstance caseBody''
      -- Trigger an avalanche
      let newStatus = case isAccident of
                        Just (Aeson.Bool True) -> CS.mobileAccident
                        _                      -> CS.mobileOrder
      updateInstance caseId (Patch.put Case.caseStatus newStatus Patch.empty)
      return caseId

  modifyResponse $ setContentType "application/json"
  writeLBS . encode $ object [ "caseId" .= caseId ]


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
  cds <- parseMayParam coords "coords"
  limit <- getParam "limit"
  brand <- getParam "car_make"
  dist  <- parseMayParam decimal "dist"
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

    sslClientCertificate' <-
      nestSnaplet "ssl-client-certificate"
        sslClientCertificate sslClientCertificateInit

    cfg <- getSnapletUserConfig

    cp <- liftIO $ lookupDefault 8000 cfg "carma_port"
    let cOpts = defaultCarmaOptions{carmaPort = cp}
        defDict = T.unpack $ modelName (modelInfo :: ModelInfo City.City)
    dName <- liftIO $ lookupDefault defDict cfg "cities-dictionary"

    addRoutes routes
    cDict' <- liftIO $ CH.runCarma cOpts $ readDictionary dName

    case cDict' of
      Nothing -> error "Could not load cities dictionary from CaRMa"
      Just cDict ->
        return $ GeoApp db sslClientCertificate' cOpts $ loadNewDict' cDict


-- | Helper for obtaining known SSL client certificates associated with proper
-- partner title which is used to mark @Case@ caller name with it.
--
-- So we're identifying partners by SSL client certificates they're using
-- and then we know exactly from which partner a @Case@ came from.
sslClientCertificateInit :: SnapletInit b SSLClientCertificate
sslClientCertificateInit = makeSnaplet snapletId snapletTitle Nothing $ do
  cfg <- getSnapletUserConfig

  !knownCertificates' <-
    liftIO (lookupDefault [] cfg "client-certificates")
      >>= mapM (\case [a, b] ->
                        liftIO (BS.readFile $ T.unpack a)
                          <&> cleanSSLCertificate
                          <&> (, b)
                      x ->
                        error $
                          "Incorrect client certificates format: " ++ show x)

  pure $ SSLClientCertificate { knownCertificates = knownCertificates' }

  where
    snapletId = "ssl-client-certificate"
    snapletTitle = "SSL Client Certificate"


obtainPartnerTitle :: ByteString -> Handler b GeoApp (Maybe Text)
obtainPartnerTitle sslClientCert =
   with sslClientCertificate get
   <&> knownCertificates
   <&> find (\(x, _) -> sslClientCert == x)
   <&> fmap snd


cleanSSLCertificate :: ByteString -> ByteString
cleanSSLCertificate = BS.filter (`notElem` ("\t\r\n " :: [Char]))


------------------------------------------------------------------------------
-- | Parse "52.32,3.45" (no spaces) into pair of doubles.
coords :: Parser (Double, Double)
coords = (,) <$> double <* anyChar <*> double


------------------------------------------------------------------------------
-- | Parse "true" or "false" into boolean.
bool :: Parser Bool
bool = (string "true" >> return True) <|> (string "false" >> return False)
