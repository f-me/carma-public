{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ApplicationHandlers where
-- FIXME: reexport AppHandlers/* & remove import AppHandlers.* from AppInit

import Prelude hiding (log)

import Data.Functor
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.CatchIO
import Control.Exception (SomeException)
import Control.Concurrent (myThreadId)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Attoparsec.Number as A
import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.=))
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.String

import Data.Maybe
import Data.Ord (comparing)

import Data.Time

import System.FilePath
import System.Locale

import Database.PostgreSQL.Simple ( Query, query_, query, execute)
import Database.PostgreSQL.Simple.SqlQQ
import qualified Snap.Snaplet.PostgresqlSimple as PS
import Data.Pool (withResource)
import Heist
import Heist.Interpreted
import Text.XmlHtml as X

import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.SimpleLog
import Snap.Util.FileServe (serveFile)
import Snap.Util.FileUploads (getMaximumFormInputSize)

import WeatherApi (getWeather', tempC)

import Snaplet.Auth.PGUsers
import qualified Snaplet.DbLayer as DB
import qualified Snaplet.DbLayer.Types as DB
import qualified Snaplet.DbLayer.ARC as ARC
import qualified Snaplet.DbLayer.RKC as RKC
import qualified Snaplet.DbLayer.Dictionary as Dict
import Snaplet.FileUpload (FileUpload(cfg), doUpload, oneUpload)

import Carma.Model
import Data.Model.CRUD
import qualified Data.Model.Patch.Sql as Patch
import Data.Model.Patch (Patch(..))

import Application
import AppHandlers.Util
import AppHandlers.Users
import Util as U hiding (render, withPG)
import Utils.NotDbLayer (readIdent)

import Carma.Model.Event (EventType(..))
import Utils.Events (logLogin, logLegacyCRUD)

import qualified Carma.Model.Usermeta as Usermeta


------------------------------------------------------------------------------
-- | Render empty form for model.
indexPage :: AppHandler ()
indexPage = ifTop $ do
    ln <- gets $ localName . options
    -- Render index page with <addLocalName> splice defined, which
    -- appends the @local-name@ config option to its argument.
    let addLocalName :: Splice AppHandler
        addLocalName = do
            t <- X.nodeText <$> getParamNode
            let r = case ln of
                      Just s  -> T.concat [t, " [", s, "]"]
                      Nothing -> t
            return $ [X.TextNode r]
        splices = do
          "addLocalName" ## addLocalName
    renderWithSplices "index" splices


------------------------------------------------------------------------------
-- | Redirect using 303 See Other to login form.
--
-- Used after unsuccessful access/login attempt or logout.
redirectToLogin :: MonadSnap m => m a
redirectToLogin = redirect' "/login" 303


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
  case res of
    Left  _ -> redirectToLogin
    Right _ -> do
      logLogin Login
      redirect "/"


doLogout :: AppHandler ()
doLogout = ifTop $ do
  claimUserLogout
  logLogin Logout
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


createHandler :: AppHandler ()
createHandler = do
  Just model <- getParam "model"
  let createModel :: forall m . Model m => m -> AppHandler Aeson.Value
      createModel _ = do
        let crud = getModelCRUD :: CRUD m
        commit <- getJSONBody :: AppHandler Aeson.Value
        s <- PS.getPostgresState
        res <- liftIO $ withResource (PS.pgPool s)
                (runEitherT . crud_create crud commit)
        case res of
          Right obj -> return obj
          Left err  -> error $ "in createHandler: " ++ show err
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
          let ident = readIdent objId :: IdentI m
          s <- PS.getPostgresState
          liftIO $ withResource (PS.pgPool s)
                     (runEitherT . crud_read getModelCRUD ident)
        case res of
          Right obj              -> writeJSON obj
          Left (NoSuchObject _)  -> handleError 404
          Left err               -> error $ "in readHandler: " ++ show err
  -- See also Utils.NotDbLayer.read
  case Carma.Model.dispatch (T.decodeUtf8 model) readModel of
    Just fn -> fn
    _ -> with db (DB.read model objId) >>= \case
      obj | Map.null obj -> handleError 404
          | otherwise    -> writeJSON obj


readManyHandler :: AppHandler ()
readManyHandler = do
  Just model  <- getParam "mdl" -- NB: this param can shadow query params
  limit  <- fromMaybe 2000 . fmap readInt <$> getParam "limit"
  offset <- fromMaybe    0 . fmap readInt <$> getParam "offset"
  params <- getQueryParams
  let queryFilter =
          [(T.decodeUtf8 k, T.decodeUtf8 v)
          | (k,v:_) <- Map.toList params
          , not $ k `elem` ["limit", "offset"]
          ]
  let readModel :: forall m . Model m => m -> AppHandler ()
      readModel _ = do
        res <- with db $ do
          s   <- PS.getPostgresState
          liftIO $ withResource
            (PS.pgPool s)
            (Patch.readManyWithFilter limit offset queryFilter)
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
  let updateModel :: forall m. Model m =>
                     m -> AppHandler (Either Int Aeson.Value)
      updateModel _ = do
        let ident = readIdent objId :: IdentI m
        commit <- getJSONBody :: AppHandler (Patch m)
        s   <- PS.getPostgresState
        res <- with db $ do
          liftIO $ withResource (PS.pgPool s) (Patch.update ident commit)
        case res of
          0 -> return $ Left 404
          _ -> case model of
                 -- TODO #1352 workaround for Contract triggers
                 "Contract" ->
                     do
                       res' <- liftIO $
                              withResource (PS.pgPool s) (Patch.read ident)
                      -- TODO Cut out fields from original commit like
                      -- DB.update does
                       case (Aeson.decode $ Aeson.encode res') of
                         Just [obj] -> return $ Right obj
                         err        -> error $
                                       "BUG in updateHandler: " ++ show err
                 -- TODO: workaround to catch delayed state updates
                 -- remove with new triggers
                 "Usermeta" -> do
                   let hmcommit = untypedPatch commit
                       legacyId = B.concat ["Usermeta:", objId]
                   when (HM.member "delayedState" hmcommit) $ do
                     void $ logLegacyCRUD Update legacyId Usermeta.delayedState
                   return $ Right $ Aeson.object []

                 _ -> return $ Right $ Aeson.object []
  -- See also Utils.NotDbLayer.update
  case Carma.Model.dispatch (T.decodeUtf8 model) updateModel of
    Just fn ->
        fn >>= \case
           Left n -> handleError n
           Right o -> logResp $ return o
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
  let tplName = B.unpack (reportInfo Map.! "templates")
  log Info $ T.concat ["Generating report ", T.pack tplName]
  let template
        = "resources/static/fileupload/report/"
        ++ (B.unpack reportId) ++ "/templates/" ++ tplName
  let result = "resources/reports/" ++ tplName
  let
    -- convert format and UTCize time, and apply f to UTCTime
    validateAnd f dateStr = fmap (format . f) $ parse dateStr where
      format = T.pack . formatTime defaultTimeLocale "%d.%m.%Y %X"
      parse :: T.Text -> Maybe UTCTime
      parse = parseTime defaultTimeLocale "%d.%m.%Y" . T.unpack
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
  f <- with fileUpload $ oneUpload =<<
       (doUpload $ "report" </> (U.bToString objId) </> "templates")
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

lookupSrvQ :: Query
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


-- | Serve parts of the application config to client in JSON.
clientConfig :: AppHandler ()
clientConfig = do
  mus <- with fileUpload $ gets (fromIntegral . getMaximumFormInputSize . cfg)
  let config :: Map.Map T.Text Aeson.Value
      config = Map.fromList [("max-file-size", Aeson.Number $ A.I mus)]
  writeJSON config


restoreProgramDefaults :: AppHandler ()
restoreProgramDefaults = do
  Just pgm <- getParam "pgm"
  withPG pg_search $ \c -> do
    validPgm <- query c
      [sql| SELECT 1 FROM "Program" WHERE id = ?::int |]
      [pgm]
    case validPgm of
      [[1::Int]] -> do
        void $ execute c
          [sql| DELETE FROM "NewCaseField" where program = ? |]
          [pgm]
        void $ execute c
          [sql|
            INSERT INTO "NewCaseField"
                (program, field, label, info, required, r, w)
              SELECT ?::int, field, label, info, required, r, w
              FROM "DefaultNewCaseField"
            |]
          [pgm]
      _ -> error $ "invalid program " ++ show pgm


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
