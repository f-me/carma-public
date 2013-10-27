{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ApplicationHandlers where
-- FIXME: reexport AppHandlers/* & remove import AppHandlers.* from AppInit

import Prelude hiding (log)

import Data.Functor
import Control.Monad
import Control.Monad.CatchIO
import Control.Exception (SomeException)
import Control.Concurrent (myThreadId)
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.=))
import Data.List
import Data.Map (Map)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String

import Data.Maybe
import Data.Ord (comparing)

import Data.Time

import System.FilePath
import System.IO
import System.Locale

import Database.PostgreSQL.Simple (Query, query_, query)
import Database.PostgreSQL.Simple.SqlQQ
import qualified Snap.Snaplet.PostgresqlSimple as PS
import Data.Pool (withResource)

import Snap
import Snap.Http.Server.Config as S
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.SimpleLog
import Snap.Snaplet.Vin
import Snap.Util.FileServe (serveFile, serveFileAs)

------------------------------------------------------------------------------
import Carma.Partner
import WeatherApi (getWeather', tempC)
-----------------------------------------------------------------------------
import Snaplet.Auth.PGUsers
import qualified Snaplet.DbLayer as DB
import qualified Snaplet.DbLayer.Types as DB
import qualified Snaplet.DbLayer.ARC as ARC
import qualified Snaplet.DbLayer.RKC as RKC
import qualified Snaplet.DbLayer.Dictionary as Dict
import Snaplet.FileUpload (tmp, doUpload)
------------------------------------------------------------------------------
import Application
import AppHandlers.Util
import AppHandlers.Users
import Util as U hiding (render)
import RuntimeFlag

import Carma.Model
import Data.Model.Patch (Patch)
import qualified Data.Model.Patch.Sql as Patch

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
  res <- with auth $ loginByUsername (T.decodeUtf8 l) (ClearText p) r
  either (const redirectToLogin) (const $ redirect "/") res


doLogout :: AppHandler ()
doLogout = ifTop $ do
  claimUserLogout
  with auth logout
  redirectToLogin


------------------------------------------------------------------------------
-- | Geodecode mockup.
smspost :: AppHandler ()
smspost = do
  Just smsId <- getParam "smsId"
  Right _ <- with db $ DB.submitTask "smspost" smsId
  writeBS ""


------------------------------------------------------------------------------
-- | CRUD

-- FIXME: this is way too slow
readInt :: (Read i, Integral i) => ByteString -> i
readInt = read . read . show

readIdent :: ByteString -> Ident m
readIdent = Ident . readInt


createHandler :: AppHandler ()
createHandler = do
  Just model <- getParam "model"
  let createModel :: forall m . Model m => m -> AppHandler Aeson.Value
      createModel _ = do
        commit <- getJSONBody :: AppHandler (Patch m)
        s <- PS.getPostgresState
        i <- liftIO $ withResource (PS.pgPool s) (Patch.create commit)
        return $ Aeson.object ["id" .= i]
  case Carma.Model.dispatch (T.decodeUtf8 model) createModel of
    Just fn -> logResp $ fn
    Nothing -> logResp $ do
      commit <- getJSONBody
      logReq commit
      with db $ DB.create model commit


readHandler :: AppHandler ()
readHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  let readModel :: forall m . Model m => m -> AppHandler ()
      readModel _ = do
        res <- with db $ do
          let ident = readIdent objId :: Ident m
          s <- PS.getPostgresState
          liftIO $ withResource (PS.pgPool s) (Patch.read ident)
        case res of
          [obj] -> writeJSON obj
          []    -> handleError 404
          _     -> error $ "BUG in readHandler: " ++ show (Aeson.encode res)
  case Carma.Model.dispatch (T.decodeUtf8 model) readModel of
    Just fn -> fn
    _ -> with db (DB.read model objId) >>= \case
      obj | Map.null obj -> handleError 404
          | otherwise    -> writeJSON obj


readManyHandler :: AppHandler ()
readManyHandler = do
  Just model  <- getParam "model"
  limit  <- fromMaybe 2000 . fmap readInt <$> getParam "limit"
  offset <- fromMaybe   0 . fmap readInt <$> getParam "offset"
  let readModel :: forall m . Model m => m -> AppHandler ()
      readModel _ = do
        res <- with db $ do
          s   <- PS.getPostgresState
          liftIO $ withResource (PS.pgPool s) (Patch.readMany limit offset)
        writeJSON (res :: [Patch m])
  case Carma.Model.dispatch (T.decodeUtf8 model) readModel of
    Just fn -> fn
    _       -> handleError 404


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
  let updateModel :: forall m . Model m => m -> AppHandler ()
      updateModel _ = do
        let ident = readIdent objId :: Ident m
        commit <- getJSONBody :: AppHandler (Patch m)
        res <- with db $ do
          s   <- PS.getPostgresState
          liftIO $ withResource (PS.pgPool s) (Patch.update ident commit)
        case res of
          0 -> handleError 404
          _ -> writeJSON $ Aeson.object []

  case Carma.Model.dispatch (T.decodeUtf8 model) updateModel of
    Just fn -> fn
    Nothing -> do
      commit <- getJSONBody
      logReq commit
      with db (DB.read model objId) >>= \case
        obj | Map.null obj -> handleError 404
            | otherwise    -> logResp $ with db
                $ DB.update model objId
                -- Need this hack, or server won't return updated "cost_counted"
                $ Map.delete "cost_counted" commit


deleteHandler :: AppHandler ()
deleteHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  res        <- with db $ DB.delete model objId
  writeJSON res

-- rkc helpers
getFromTo :: AppHandler (Maybe UTCTime, Maybe UTCTime)
getFromTo = do
  fromTime <- getParam "from"
  toTime <- getParam "to"

  tz <- liftIO getCurrentTimeZone

  let
    parseLocalTime :: ByteString -> Maybe LocalTime
    parseLocalTime = parseTime defaultTimeLocale "%d.%m.%Y" . U.bToString

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

  usrs <- with db usersListPG
  info <- with db $ RKC.rkc usrs flt'
  writeJSON info

rkcWeatherHandler :: AppHandler ()
rkcWeatherHandler = scope "rkc" $ scope "handler" $ scope "weather" $ do
  let defaults = ["Moskva", "Sankt-Peterburg"]
  cities <- (fromMaybe defaults . (>>= (Aeson.decode . LB.fromStrict)))
    <$> getParam "cities"

  log Trace $ T.concat ["Cities: ", fromString $ intercalate ", " cities]

  conf <- with db $ gets DB.weather
  let weatherForCity = liftIO . getWeather' conf . filter (/= '\'')
  let toTemp t city = Aeson.object [
        "city" .= city,
        "temp" .= either (const "-") (show.tempC) t]

  temps <- mapM weatherForCity cities
  writeJSON $ Aeson.object [
    "weather" .= zipWith toTemp temps cities]



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


arcReportHandler :: AppHandler ()
arcReportHandler = scope "arc" $ scope "handler" $ do
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
  let Just objId = Map.lookup "id" res
  f <- with fileUpload $ 
       doUpload $ "report" </> (U.bToString objId) </> "templates"
  Just name  <- getParam "name"
  -- we have to update all model params after fileupload,
  -- because in multipart/form-data requests we do not have
  -- params as usual, see Snap.Util.FileUploads.setProcessFormInputs
  _ <- with db $ DB.update "report" objId $
    Map.fromList [ ("templates", T.encodeUtf8 $ T.pack $ takeFileName f)
                 , ("name",      name) ]
  redirect "/#reports"

deleteReportHandler :: AppHandler ()
deleteReportHandler = do
  Just objId  <- getParam "id"
  with db $ DB.delete "report" objId
  -- TODO Rewrite this using CRUD triggers
  -- with fileUpload $ doDeleteAll' "report" $ U.bToString objId

serveUsersList :: AppHandler ()
serveUsersList = with db usersListPG >>= writeJSON


------------------------------------------------------------------------------
-- | Utility functions
vinUploadData :: AppHandler ()
vinUploadData = scope "vin" $ scope "upload" $ do
  log Trace "Uploading data"
  fPath <- with fileUpload $ doUpload "vin-upload-data"

  let fName = takeFileName fPath

  log Trace $ T.concat ["Uploaded to file: ", T.pack fName]
  prog <- getParam "program"
  case prog of
    Nothing -> log Error "Program not specified"
    Just pgmId -> do
      log Info $ T.concat ["Uploading ", T.pack fName]
      log Trace $ T.concat ["Program: ", T.decodeUtf8 pgmId]

      -- Check if this program is available for user
      Just u <- with auth currentUser
      u' <- with db $ replaceMetaRolesFromPG u
      let Aeson.String userPgms' = HM.lookupDefault "" "programs" $ userMeta u'
          userPgms = B.split ',' $ T.encodeUtf8 userPgms'
      when (not $ 
            (elem (Role "partner") (userRoles u') && elem pgmId userPgms) ||
            (elem (Role "programman") (userRoles u'))) $
            handleError 403

      -- Find out which format is used for this program
      progObj <- with db $ DB.read "program" pgmId
      let vF = Map.findWithDefault "" "vinFormat" progObj

      log Trace $ T.concat ["VIN format: ", T.decodeUtf8 vF]

      log Trace $ T.concat ["Initializing state for file: ", T.pack fName]
      with vin $ initUploadState fName
      log Trace $ T.concat ["Uploading data from file: ", T.pack fName]

      -- Set current user as owner
      let Just (UserId uid) = userId u
      with vin $ uploadData (T.encodeUtf8 uid) pgmId (T.unpack . T.decodeUtf8 $ vF) fPath

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


-- | Upload a CSV file and update the partner database, serving a
-- report back to the client.
--
-- (carma-partner-import package interface).
partnerUploadData :: AppHandler ()
partnerUploadData = scope "partner" $ scope "upload" $ do
  sCfg <- liftIO $ commandLineConfig (emptyConfig :: S.Config Snap a)
  let carmaPort = case getPort sCfg of
                    Just n -> n
                    Nothing -> error "No port"
  tmpPath <- with fileUpload $ gets tmp
  (tmpName, _) <- liftIO $ openTempFile tmpPath "last-pimp.csv"

  log Trace "Uploading data"
  inPath <- with fileUpload $ doUpload "partner-upload-data"

  let outPath = tmpPath </> tmpName

  log Trace $ T.pack $ "Input file " ++ inPath
  log Trace $ T.pack $ "Output file " ++ outPath

  log Trace "Loading dictionaries from CaRMa"
  Just dicts <- liftIO $ loadIntegrationDicts carmaPort
  log Trace "Processing data"
  liftIO $ processData carmaPort inPath outPath dicts
  log Trace "Serve processing report"
  serveFileAs "text/csv" outPath


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

-- | Calculate average tower arrival time (in seconds) for today,
-- parametrized by city (a value from DealerCities dictionary).
towAvgTimeQuery :: Query
towAvgTimeQuery = [sql|
WITH towtimes AS (
 SELECT max(t.times_factServiceStart - a.ctime)
 FROM actiontbl a, casetbl c, towagetbl t
 WHERE cast(split_part(a.parentid, ':', 2) as integer)=t.id
 AND cast(split_part(a.caseid, ':', 2) as integer)=c.id
 AND a.name='orderService'
 AND c.city=?
 AND (CURRENT_DATE, INTERVAL '1 day') OVERLAPS (c.callDate, c.callDate)
 GROUP BY a.parentid)
SELECT extract(epoch from avg(max)) FROM towtimes;
|]

-- | Read city name from @city@ request parameter and return results
-- of 'towAvgTime' query for that city as a single-element JSON list
-- (possibly containing @null@ if the time cannot be calculated).
towAvgTime :: AppHandler ()
towAvgTime = do
  city <- getParam "city"
  case city of
    Just c -> do
          rows <- withPG pg_search $
                  \conn -> query conn towAvgTimeQuery [c]
          writeJSON (map head rows :: [Maybe Double])
    _ -> error "Could not read city from request"


getRegionByCity :: AppHandler ()
getRegionByCity = do
  getParam "city" >>= \case
    Just city -> do
      res <- withPG pg_search $ \c -> query c
        [sql|
          SELECT r.label
          FROM "Region" r, "City" c
          WHERE c.id = ANY(r.cities) AND c.value = ?
        |]
        [city]
      writeJSON (res :: [[ByteString]])
    _ -> error "Could not read city from request"


smsProcessingHandler :: AppHandler ()
smsProcessingHandler = scope "sms" $ do
  res <- with db DB.smsProcessing
  writeJSON $ object [
    "processing" .= res]

-- | Read @actionid@ request parameter and set @openTime@ of that
-- action to current time.
openAction :: AppHandler ()
openAction = do
  aid <- getParam "actionid"
  case aid of
    Nothing -> error "Could not read actionid parameter"
    Just i -> do
      dn <- liftIO $ projNow id
      res <- with db $ DB.update "action" i $
                       Map.singleton "openTime" dn
      writeJSON res

lookupSrvQ = [sql|
  SELECT c.id::text
       , c.comment
       , c.owner
       , c.partnerid
       , (extract (epoch from c.ctime at time zone 'UTC')::int8)::text
       , c.partnercancelreason
       , p.name
       , c.serviceid
  FROM partnercanceltbl c
  LEFT JOIN partnertbl p
  ON p.id::text = substring(c.partnerid, ':(.*)')
  WHERE c.serviceid = ?
|]

printServiceHandler :: AppHandler ()
printServiceHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  srv     <- with db $ DB.read model objId
  kase    <- with db $ DB.read' $ fromMaybe "" $ Map.lookup "parentId" srv
  actions <- with db $ mapM DB.read' $
             B.split ',' $ Map.findWithDefault "" "actions" kase
  let modelId = B.concat [model, ":", objId]
      action  = head' $ filter ((Just modelId ==) . Map.lookup "parentId")
                      $ actions
  rows <- withPG pg_search $ \conn -> query conn lookupSrvQ [modelId]
  writeJSON $ Map.fromList [ ("action" :: ByteString, [action])
                           , ("kase",    [kase])
                           , ("service", [srv])
                           , ("cancels", cancelMap rows)
                           ]
    where
      head' []     = Map.empty
      head' (x:_)  = x
      cancelMap rows = mkMap [ "id"
                             , "comment"
                             , "owner"
                             , "partnerid"
                             , "ctime"
                             , "partnerCancelReason"
                             , "partnerName"
                             , "serviceid"
                             ] rows

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
  log Info $ lb2t' r

unassignedActionsHandler :: AppHandler ()
unassignedActionsHandler = do
  r <- withPG pg_search
       $ \c -> query_ c $ fromString
               $  " SELECT count(1) FROM actiontbl"
               ++ " WHERE name IN ('orderService', 'callMeMaybe', 'tellMeMore')"
               ++ " AND (assignedTo = '' OR assignedTo is null)"
               ++ " AND closed = false"
  writeJSON $ join (r :: [[Integer]])

lb2t' :: LB.ByteString -> T.Text
lb2t' = T.decodeUtf8With T.lenientDecode . LB.toStrict

logReq :: Aeson.ToJSON v => v -> AppHandler ()
logReq commit  = do
  user <- fmap userLogin <$> with auth currentUser
  r <- getRequest
  thId <- liftIO myThreadId
  let params = rqParams r
      uri    = rqURI r
      rmethod = rqMethod r
  scope "detail" $ scope "req" $ log Trace $ lb2t' $ Aeson.encode $ object [
    "threadId" .= show thId,
    "request" .= object [
      "user" .= user,
      "method" .= show rmethod,
      "uri" .= uri,
      "params" .= params,
      "body" .= commit]]

logResp :: Aeson.ToJSON v => AppHandler v -> AppHandler ()
logResp act = runAct `catch` logFail where
  runAct = do
    r' <- act
    scope "detail" $ scope "resp" $ do
      thId <- liftIO myThreadId
      log Trace $ lb2t' $ Aeson.encode $ object [
        "threadId" .= show thId,
        "response" .= r']
      writeJSON r'
  logFail :: SomeException -> AppHandler ()
  logFail e = do
    scope "detail" $ scope "resp" $ do
      thId <- liftIO myThreadId
      log Trace $ lb2t' $ Aeson.encode $ object [
        "threadId" .= show thId,
        "response" .= object ["error" .= show e]]
    throw e
