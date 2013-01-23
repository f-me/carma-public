{-# LANGUAGE DoAndIfThenElse, OverloadedStrings #-}

module ApplicationHandlers where
-- FIXME: reexport AppHandlers/* & remove import AppHandlers.* from AppInit

import Prelude hiding (log)

import Data.Functor
import Control.Monad
import Control.Monad.CatchIO
import Control.Concurrent.STM

import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as B (toStrict)
import qualified Data.ByteString.UTF8  as BU
import qualified Data.Aeson as Aeson
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String
import qualified Data.HashMap.Strict as HashMap

import Data.Maybe
import Data.Ord (comparing)

import Data.Time

import Data.Aeson (object, (.=))

import System.Locale

import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.SimpleLog
import Snap.Snaplet.Vin
import Snap.Util.FileServe (serveFile)

import WeatherApi (getWeather', tempC, Config)
------------------------------------------------------------------------------
import qualified Snaplet.DbLayer as DB
import qualified Snaplet.DbLayer.Types as DB
import qualified Snaplet.DbLayer.ARC as ARC
import qualified Snaplet.DbLayer.RKC as RKC
import qualified Snaplet.DbLayer.Dictionary as Dict
import Snaplet.FileUpload (doUpload', doDeleteAll')
------------------------------------------------------------------------------
import qualified Nominatim
-----------------------------------------------------------------------------
import Application
import AppHandlers.Util
import Util
import RuntimeFlag


------------------------------------------------------------------------------
-- | Render empty form for model.
indexPage :: AppHandler ()
indexPage = ifTop $ render "index"


------------------------------------------------------------------------------
-- | Redirect using 303 See Other to login form.
--
-- Used after unsuccessful access/login attempt or logout.
redirectToLogin :: MonadSnap m => m a
redirectToLogin = redirect' "/login/" 303


------------------------------------------------------------------------------
-- | If user is not logged in, redirect to login page, pass to
-- handler otherwise.
authOrLogin :: AppHandler () -> AppHandler ()
authOrLogin = requireUser auth redirectToLogin


------------------------------------------------------------------------------
-- | Render empty login form.
loginForm :: AppHandler ()
loginForm = serveFile "snaplets/heist/resources/templates/login.html"


------------------------------------------------------------------------------
-- | Login user.
doLogin :: AppHandler ()
doLogin = ifTop $ do
  l <- fromMaybe "" <$> getParam "login"
  p <- fromMaybe "" <$> getParam "password"
  r <- isJust <$> getParam "remember"
  res <- with auth $ loginByUsername l (ClearText p) r
  case res of
    Left _ -> redirectToLogin
    Right u -> do
      creds <- (,)
        <$> getParam "avayaExt"
        <*> getParam "avayaPwd"
      _ <- case creds of
        ~(Just ext, Just pwd) -> with auth $ saveUser
          u {userMeta
            = HashMap.insert "avayaExt" (Aeson.toJSON ext)
            $ HashMap.insert "avayaPwd" (Aeson.toJSON pwd)
            $ userMeta u
            }
      _ <- addToLoggedUsers u

      redirect "/"


doLogout :: AppHandler ()
doLogout = ifTop $ do
  Just u <- with auth currentUser
  rmFromLoggedUsers u
  with auth logout
  redirectToLogin

------------------------------------------------------------------------------
-- | Serve user account data back to client.
serveUserCake :: AppHandler ()
serveUserCake = ifTop
  $ with auth currentUser
  >>= maybe (error "impossible happened") writeJSON


------------------------------------------------------------------------------
-- | Geodecode mockup.
smspost :: AppHandler ()
smspost = do
  Just smsId <- getParam "smsId"
  Right _ <- with db $ DB.submitTask "smspost" smsId
  writeBS ""


------------------------------------------------------------------------------
-- | Geodecode mockup.
geodecode :: AppHandler ()
geodecode = ifTop $ do
  addr <- fromMaybe "Moscow" <$> getParam "addr"
  resp <- liftIO $ Nominatim.geodecode addr
  writeJSON resp

------------------------------------------------------------------------------
-- | CRUD
createHandler :: AppHandler ()
createHandler = do
  Just model <- getParam "model"
  commit <- getJSONBody
  logReq commit
  res <- with db $ DB.create model commit
  -- FIXME: try/catch & handle/log error
  logResp res

readHandler :: AppHandler ()
readHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  res <- with db $ DB.read model objId
  -- FIXME: try/catch & handle/log error
  writeJSON res

readAllHandler :: AppHandler ()
readAllHandler = do
  Just model <- getParam "model"
  (with db $ DB.readAll model)
    >>= apply "orderby" sortBy (flip . comparing . Map.lookup)
    >>= apply "limit"   take   (read . B.unpack)
    >>= apply "select"  filter flt
    >>= apply "fields"  map    proj
    >>= writeJSON
  where
    apply name f g = \xs
      -> maybe xs (\p -> f (g p) xs)
      <$> getParam name

    proj fs = \obj -> Map.fromList
      [(k, Map.findWithDefault "" k obj)
      | k <- B.split ',' fs
      ]

    flt prm = \obj -> all (selectParse obj) $ B.split ',' prm

updateHandler :: AppHandler ()
updateHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  commit <- getJSONBody
  logReq commit
  -- Need this hack, or server won't return updated "cost_counted"
  res <- with db $ DB.update model objId $ Map.delete "cost_counted" commit
  -- FIXME: try/catch & handle/log error
  logResp res

deleteHandler :: AppHandler ()
deleteHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  res        <- with db $ DB.delete model objId
  writeJSON res

searchHandler :: AppHandler ()
searchHandler = scope "searchHandler" $ do
  Just q <- getParam "q"
  Just m <- getParam "model"
  Just fs <- getParam "fields"
  Just sel <- getParam "select"
  let
    getInt v = do
      s <- v
      (x, _) <- B.readInt s
      return x
    sels = B.split ',' sel
  lim <- liftM (maybe 100 id . getInt) $ getParam "limit"
  res <- with db $ DB.searchFullText m (B.split ',' fs) sels q lim
  writeJSON $ map (Map.fromList . zip sels) res

-- rkc helpers
getFromTo :: AppHandler (Maybe UTCTime, Maybe UTCTime)
getFromTo = do
  fromTime <- getParam "from"
  toTime <- getParam "to"

  tz <- liftIO getCurrentTimeZone

  let
    parseLocalTime :: ByteString -> Maybe LocalTime
    parseLocalTime = parseTime defaultTimeLocale "%d.%m.%Y" . BU.toString

    fromTime' = fmap (localTimeToUTC tz) (fromTime >>= parseLocalTime)
    toTime' = fmap (localTimeToUTC tz) (toTime >>= parseLocalTime)

  return (fromTime', toTime')

getParamOrEmpty :: ByteString -> AppHandler T.Text
getParamOrEmpty = liftM (maybe T.empty T.decodeUtf8) . getParam

rkcHandler :: AppHandler ()
rkcHandler = scope "rkc" $ scope "handler" $ do
  p <- getParamOrEmpty "program"
  c <- getParamOrEmpty "city"
  part <- getParamOrEmpty "partner"
  (from, to) <- getFromTo

  flt <- liftIO RKC.todayFilter
  let
    flt' = flt {
      RKC.filterFrom = fromMaybe (RKC.filterFrom flt) from,
      RKC.filterTo = fromMaybe (RKC.filterTo flt) to,
      RKC.filterProgram = p,
      RKC.filterCity = c,
      RKC.filterPartner = part }

  usrs <- gets allUsers
  info <- with db $ RKC.rkc usrs flt'
  writeJSON info

rkcWeatherHandler :: AppHandler ()
rkcWeatherHandler = scope "rkc" $ scope "handler" $ scope "weather" $ do
  Just u <- with auth currentUser
  cities <- case HashMap.lookup "weathercities" (userMeta u) of
    Nothing -> return defaultCities
    Just cities' -> case Aeson.fromJSON cities' of
      Aeson.Success r -> return r
      Aeson.Error e -> do
        log Error "Can't read weather cities"
        return defaultCities

  toRemove <- liftM (maybeToList . fmap BU.toString) $ getParam "remove"
  toAdd <- liftM (maybeToList . fmap BU.toString) $ getParam "add"

  let
    newCities = nub $ (cities ++ toAdd) \\ toRemove

  with auth $ saveUser $ u {
    userMeta = HashMap.insert "weathercities" (Aeson.toJSON newCities) (userMeta u) }

  log Trace $ T.concat ["Cities: ", fromString $ intercalate ", " newCities]
  conf <- with db $ gets DB.weather

  temps <- mapM (liftIO . weatherForCity conf) newCities
  writeJSON $ Aeson.object [
    "weathers" .= zipWith toTemp temps newCities]

  where
    weatherForCity :: WeatherApi.Config -> String -> IO (Either String Double)
    weatherForCity conf city = liftM (either (Left . show) (Right . tempC)) $
      getWeather' conf city

    defaultCities :: [String]
    defaultCities = ["Moskva", "Sankt-Peterburg"]

    toTemp :: Either String Double -> String -> Aeson.Value
    toTemp t city = Aeson.object [
      "city" .= city,
      "temp" .= either (const "-") show t]

rkcFrontHandler :: AppHandler ()
rkcFrontHandler = scope "rkc" $ scope "handler" $ scope "front" $ do
  p <- getParamOrEmpty "program"
  c <- getParamOrEmpty "city"
  part <- getParamOrEmpty "partner"
  (from, to) <- getFromTo

  flt <- liftIO RKC.todayFilter
  let
    flt' = flt {
      RKC.filterFrom = fromMaybe (RKC.filterFrom flt) from,
      RKC.filterTo = fromMaybe (RKC.filterTo flt) to,
      RKC.filterProgram = p,
      RKC.filterCity = c,
      RKC.filterPartner = part }

  res <- with db $ RKC.rkcFront flt'
  writeJSON res

rkcPartners :: AppHandler ()
rkcPartners = scope "rkc" $ scope "handler" $ scope "partners" $ do
  flt <- liftIO RKC.todayFilter
  (from, to) <- getFromTo

  let
    flt' = flt {
      RKC.filterFrom = fromMaybe (RKC.filterFrom flt) from,
      RKC.filterTo = fromMaybe (RKC.filterTo flt) to }

  res <- with db $ RKC.partners (RKC.filterFrom flt') (RKC.filterTo flt')
  writeJSON res

logtest :: AppHandler ()
logtest = do
  r <- getRequest
  log Fatal $ T.decodeUtf8 $ rqURI r

arcReportHandler :: AppHandler ()
arcReportHandler = scope "arc" $ scope "handler" $ do
  logtest
  year <- tryParam B.readInteger "year"
  month <- tryParam B.readInt "month"
  dicts <- scope "dictionaries" . Dict.loadDictionaries $ "resources/site-config/dictionaries"
  with db $ ARC.arcReport dicts year month
  serveFile "ARC.xlsx"
  where
    tryParam :: MonadSnap m => (ByteString -> Maybe (a, ByteString)) -> ByteString -> m a
    tryParam reader name = do
      bs <- getParam name
      case bs >>= reader of
        Nothing -> error $ "Unable to parse " ++ B.unpack name
        Just (v, "") -> return v
        Just (_, _) -> error $ "Unable to parse " ++ B.unpack name

-- | This action recieve model and id as parameters to lookup for
-- and json object with values to create new model with specified
-- id when it's not found
findOrCreateHandler :: AppHandler ()
findOrCreateHandler = do
  Just model <- getParam "model"
  Just objId    <- getParam "id"
  commit <- getJSONBody
  res <- with db $ DB.findOrCreate model objId commit
  -- FIXME: try/catch & handle/log error
  writeJSON res

------------------------------------------------------------------------------
-- | Reports
report :: AppHandler ()
report = scope "report" $ do
  extendTimeout 6000
  Just reportId <- getParam "program"
  fromDate <- liftM (fmap T.decodeUtf8) $ getParam "from"
  toDate <- liftM (fmap T.decodeUtf8) $ getParam "to"
  reportInfo <- with db $ DB.read "report" reportId
  tz <- liftIO getCurrentTimeZone
  let tplName = B.unpack (reportInfo Map.! "templates")
  log Info $ T.concat ["Generating report ", T.pack tplName]
  let template
        = "resources/static/fileupload/report/"
        ++ (B.unpack reportId) ++ "/templates/" ++ tplName
  let result = "resources/reports/" ++ tplName
  let
    -- convert format and UTCize time, and apply f to UTCTime
    validateAnd f dateStr = fmap (format . f . toUTC) $ parse dateStr where
      format = T.pack . formatTime defaultTimeLocale "%d.%m.%Y %X"
      parse :: T.Text -> Maybe LocalTime
      parse = parseTime defaultTimeLocale "%d.%m.%Y" . T.unpack
      toUTC = localTimeToUTC tz
    withinAnd f pre post dateValue = do
      v <- dateValue
      s <- validateAnd f v
      return $ T.concat [T.pack pre, s, T.pack post]

    fromTo mdl = (from, to) where
      from = withinAnd id (mdl ++ " >= to_timestamp('") "', 'DD.MM.YYYY HH24:MI:SS')" fromDate
      to = withinAnd addDay (mdl ++ " < to_timestamp('") "', 'DD.MM.YYYY HH24:MI:SS')" toDate

    addDay tm = tm { utctDay = addDays 1 (utctDay tm) }
    
    mkCondition nm = catMaybes [from', to'] where
      (from', to') = fromTo (T.unpack nm)
  
  with db $ DB.generateReport mkCondition template result
  modifyResponse $ addHeader "Content-Disposition" "attachment; filename=\"report.xlsx\""
  serveFile result

createReportHandler :: AppHandler ()
createReportHandler = do
  res <- with db $ DB.create "report" $ Map.empty
  let objId = last $ B.split ':' $ fromJust $ Map.lookup "id" res
  (f:_)      <- with fileUpload $ doUpload' "report" objId "templates"
  Just name  <- getParam "name"
  -- we have to update all model params after fileupload,
  -- because in multipart/form-data requests we do not have
  -- params as usual, see Snap.Util.FileUploads.setProcessFormInputs
  _ <- with db $ DB.update "report" objId $
    Map.fromList [ ("templates", BU.fromString f)
                 , ("name",      name) ]
  redirect "/#reports"

deleteReportHandler :: AppHandler ()
deleteReportHandler = do
  Just objId  <- getParam "id"
  with db $ DB.delete "report" objId
  with fileUpload $ doDeleteAll' "report" objId
  return ()

getUsersDict :: AppHandler ()
getUsersDict = writeJSON =<< gets allUsers

getActiveUsers :: AppHandler ()
getActiveUsers = do
  tvar <- gets loggedUsers
  logdUsers <- liftIO $ readTVarIO tvar
  writeJSON $ Map.keys logdUsers


------------------------------------------------------------------------------
-- | Utility functions
vinUploadData :: AppHandler ()
vinUploadData = scope "vin" $ scope "upload" $ do
  log Trace "Uploading data"
  (f:_) <- with fileUpload $ doUpload' "report" "upload" "data"
  log Trace $ T.concat ["Uploaded to file: ", T.pack f]
  prog <- getParam "program"
  case prog of
    Nothing -> log Error "Program not specified"
    Just p -> do
      log Info $ T.concat ["Uploading ", T.pack f]
      log Trace $ T.concat ["Program: ", T.decodeUtf8 p]
      log Trace $ T.concat ["Initializing state for file: ", T.pack f]
      with vin $ initUploadState f
      log Trace $ T.concat ["Uploading data from file: ", T.pack f]
      with vin $ uploadData (T.unpack . T.decodeUtf8 $ p) f

vinStateRead :: AppHandler ()
vinStateRead = scope "vin" $ scope "state" $ scope "get" $ do
  log Trace "Getting state"
  with vin getState

vinStateRemove :: AppHandler ()
vinStateRemove = scope "vin" $ scope "state" $ scope "remove" $ do
  log Trace "Remove alert by id"
  res <- getParam "id"
  log Trace $ T.concat ["id: ", maybe "<null>" (T.pack . show) res]
  with vin removeAlert

getSrvTarifOptions :: AppHandler ()
getSrvTarifOptions = do
  Just objId    <- getParam "id"
  Just model <- getParam "model"
  srv     <- with db $ DB.read model objId
  partner <- with db $ getObj $ B.split ':' $
             fromMaybe "" $ Map.lookup "contractor_partnerId" srv
  -- partner services with same serviceName as current service model
  partnerSrvs <- with db $ mapM getObj $ getIds "services" partner
  case filter (mSrv model) partnerSrvs of
    []     -> return ()
    (x:_) -> do
      tarifOptions <- with db $ mapM getObj $ getIds "tarifOptions" x
      writeJSON $ map rebuilOpt tarifOptions
  where
      getIds f m = map (B.split ':') $ B.split ',' $
                   fromMaybe "" $ Map.lookup f m
      getObj [m, objId] = Map.insert "id" objId <$> DB.read m objId
      getObj _       = return $ Map.empty
      mSrv m = (m ==) . fromMaybe "" . Map.lookup "serviceName"
      rebuilOpt :: Map ByteString ByteString -> Map ByteString ByteString
      rebuilOpt o = Map.fromList $
                    [("id"        , fromMaybe "" $ Map.lookup "id" o)
                    ,("optionName", fromMaybe "" $ Map.lookup "optionName" o)]

smsProcessingHandler :: AppHandler ()
smsProcessingHandler = scope "sms" $ do
  res <- with db DB.smsProcessing
  writeJSON $ object [
    "processing" .= res]

printServiceHandler :: AppHandler ()
printServiceHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  srv     <- with db $ DB.read model objId
  kase    <- with db $ DB.read' $ fromJust $ Map.lookup "parentId" srv
  actions <- with db $ mapM DB.read' $
             B.split ',' $ Map.findWithDefault "" "actions" kase
  let modelId = B.concat [model, ":", objId]
      action = head' $ filter ((Just modelId ==) . Map.lookup "parentId") $ actions
  writeJSON $ Map.fromList [ ("action" :: ByteString, action)
                           , ("kase",   kase)
                           , ("service", srv)
                           ]
    where
      head' []     = Map.empty
      head' (x:_) = x


getRuntimeFlags :: AppHandler ()
getRuntimeFlags
  = gets runtimeFlags
  >>= liftIO . readTVarIO
  >>= writeJSON . map show . Set.elems


setRuntimeFlags :: AppHandler ()
setRuntimeFlags = do
  flags <- getJSONBody
  gets runtimeFlags
    >>= liftIO . atomically
    . (`modifyTVar'` updAll flags)
  getRuntimeFlags
  where
    updAll :: Map String Bool -> RuntimeFlags -> RuntimeFlags
    updAll flags s = foldl' upd s $ Map.toList flags
    upd s (k,True)  = Set.insert (read k) s
    upd s (k,False) = Set.delete (read k) s
    -- upd _ kv = error $ "Unexpected runtime flag: " ++ show kv


errorsHandler :: AppHandler ()
errorsHandler = do
  l <- gets feLog
  r <- readRequestBody 4096
  liftIO $ withLog l $ scope "frontend" $ do
  log Info $ toStrict $ decodeUtf8 r

logReq :: Aeson.ToJSON v => v -> AppHandler ()
logReq commit  = do
  user <- fmap userLogin <$> with auth currentUser
  r <- getRequest
  let params = rqParams r
      uri    = rqURI r
      rmethod = rqMethod r
  scoper "reqlogger" $ log Trace $ T.decodeUtf8 $ B.toStrict $ Aeson.encode $ object [
    "user" .= user,
    "method" .= show rmethod,
    "uri" .= uri,
    "params" .= params,
    "body" .= commit]

logResp :: Aeson.ToJSON v => v -> AppHandler ()
logResp r = scope "resplogger" $ do
  log Trace $ T.decodeUtf8 $ B.toStrict $ Aeson.encode r
  writeJSON r

------------------------------------------------------------------------------
-- | Deny requests from non-local unauthorized users.
chkAuth :: AppHandler () -> AppHandler ()
chkAuth f = do
  req <- getRequest
  if rqRemoteAddr req /= rqLocalAddr req
  then with auth currentUser >>= maybe
      (handleError 401)
      (\u -> addToLoggedUsers u >> f)
  else f


handleError :: MonadSnap m => Int -> m ()
handleError err = do
    modifyResponse $ setResponseCode err
    getResponse >>= finishWith
