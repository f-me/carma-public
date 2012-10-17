{-# LANGUAGE OverloadedStrings #-}

module ApplicationHandlers where

import Prelude hiding (log)

import Data.Functor
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String (fromString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8  as BU
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap

import Data.Maybe
import Data.List (foldl',sortBy)
import Data.Ord (comparing)

import Data.Time
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import System.Locale

import Snap
import Snap.Core
import Snap.Snaplet (with)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.Session
import Snap.Snaplet.SimpleLog
import Snap.Snaplet.Vin
import Snap.Util.FileServe (serveFile)
import Snap.Util.Readable (fromBS)
------------------------------------------------------------------------------
import qualified Snaplet.DbLayer as DB
import qualified Snaplet.DbLayer.RKC as RKC
import Snaplet.FileUpload (doUpload', doDeleteAll')
------------------------------------------------------------------------------
import qualified Nominatim
-----------------------------------------------------------------------------
import Application
import CustomLogic.ActionAssignment
import Util


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
authOrLogin h = requireUser auth redirectToLogin h


------------------------------------------------------------------------------
-- | Render empty login form.
loginForm :: AppHandler ()
loginForm = do
  serveFile $ "snaplets/heist/resources/templates/login.html"


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
      case creds of
        (Just ext, Just pwd) -> with auth $ saveUser
          u {userMeta
            = HashMap.insert "avayaExt" (Aeson.toJSON ext)
            $ HashMap.insert "avayaPwd" (Aeson.toJSON pwd)
            $ userMeta u
            }
      addToLoggedUsers u

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
  res <- with db $ DB.create model commit
  -- FIXME: try/catch & handle/log error
  writeJSON res

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
  -- Need this hack, or server won't return updated "cost_counted"
  res <- with db $ DB.update model objId $ Map.delete "cost_counted" commit
  -- FIXME: try/catch & handle/log error
  writeJSON res

deleteHandler :: AppHandler ()
deleteHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  res        <- with db $ DB.delete model objId
  writeJSON res

syncHandler :: AppHandler ()
syncHandler = scope "sync" $ do
  mdl <- getParam "model"
  from <- liftM (fmap (maybe 0 fst . B.readInt)) $ getParam "from"
  log Info $ T.concat ["Syncing ", maybe "all" T.decodeUtf8 mdl, " model(s) starting from id ", maybe "1" (fromString . show) from]
  res <- with db $ DB.sync mdl from
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

rkcHandler :: AppHandler ()
rkcHandler = scope "rkcHandler" $ do
  p <- getParam "program"
  info <- with db . RKC.rkc . maybe T.empty T.decodeUtf8 $ p
  writeJSON info

myActionsHandler :: AppHandler ()
myActionsHandler = do
  Just cUsr <- with auth currentUser
  let uLogin = userLogin cUsr
  logdUsers <- addToLoggedUsers cUsr

  actLock <- gets actionsLock
  do -- bracket_
    (liftIO $ atomically $ takeTMVar actLock)
    actions <- filter ((== Just "0") . Map.lookup "closed")
           <$> with db (DB.readAll "action")
    now <- liftIO getCurrentTime
    let (newActions,oldActions) = assignActions now actions (Map.map snd logdUsers)
    let myNewActions = take 5 $ Map.findWithDefault [] uLogin newActions
    with db $ forM_ myNewActions $ \act ->
      case Map.lookup "id" act of
        Nothing -> return ()
        Just actId -> void $ DB.update "action"
          (last $ B.split ':' actId)
          $ Map.singleton "assignedTo" $ T.encodeUtf8 uLogin
    let myOldActions = Map.findWithDefault [] uLogin oldActions
    (liftIO $ atomically $ putTMVar actLock ())
    writeJSON $ myNewActions ++ myOldActions


searchCallsByPhone :: AppHandler ()
searchCallsByPhone = do
  r <- getRequest
  calls <- with db $ DB.readAll "call"
  let phone = last $ B.split '/' (rqURI r)
  writeJSON $
    filter ((phone ==) . (Map.findWithDefault "" "callerName_phone1")) calls

getActionsForCase :: AppHandler ()
getActionsForCase = do
  Just id <- getParam "id"
  actions <- with db $ DB.readAll "action"
  let id' = B.append "case:" id
  writeJSON $
    filter ((id' ==) . (Map.findWithDefault "" "caseId")) actions

-- | This action recieve model and id as parameters to lookup for
-- and json object with values to create new model with specified
-- id when it's not found
findOrCreateHandler :: AppHandler ()
findOrCreateHandler = do
  Just model <- getParam "model"
  Just id    <- getParam "id"
  commit <- getJSONBody
  res <- with db $ DB.findOrCreate model id commit
  -- FIXME: try/catch & handle/log error
  writeJSON res

------------------------------------------------------------------------------
-- | Reports
report :: AppHandler ()
report = scope "report" $ do
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
    (fromDate', toDate') = fromTo "case"
    (fromDate'', toDate'') = fromTo "call"

    fromTo mdl = (from, to) where
      from = withinAnd id (mdl ++ ".callDate >= to_timestamp('") "', 'DD.MM.YYYY HH24:MI:SS')" fromDate
      to = withinAnd addDay (mdl ++ ".callDate < to_timestamp('") "', 'DD.MM.YYYY HH24:MI:SS')" toDate

    addDay tm = tm { utctDay = addDays 1 (utctDay tm) }
    
    dateConditions = catMaybes [fromDate', toDate', fromDate'', toDate'']
  with db $ DB.generateReport dateConditions template result
  modifyResponse $ addHeader "Content-Disposition" "attachment; filename=\"report.xlsx\""
  serveFile result

createReportHandler :: AppHandler ()
createReportHandler = do
  res <- with db $ DB.create "report" $ Map.empty
  let id = last $ B.split ':' $ fromJust $ Map.lookup "id" res
  (f:_)      <- with fileUpload $ doUpload' "report" id "templates"
  Just name  <- getParam "name"
  -- we have to update all model params after fileupload,
  -- because in multipart/form-data requests we do not have
  -- params as usual, see Snap.Util.FileUploads.setProcessFormInputs
  with db $ DB.update "report" id $
    Map.fromList [ ("templates", BU.fromString f)
                 , ("name",      name) ]
  redirect "/#reports"

deleteReportHandler :: AppHandler ()
deleteReportHandler = do
  Just id  <- getParam "id"
  with db $ DB.delete "report" id
  with fileUpload $ doDeleteAll' "report" id
  return ()

getUsersDict :: AppHandler ()
getUsersDict = writeJSON =<< gets allUsers

------------------------------------------------------------------------------
-- | Utility functions
writeJSON :: Aeson.ToJSON v => v -> AppHandler ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v

getJSONBody :: Aeson.FromJSON v => AppHandler v
getJSONBody = Util.readJSONfromLBS <$> readRequestBody 4096


addToLoggedUsers :: AuthUser -> AppHandler (Map Text (UTCTime,AuthUser))
addToLoggedUsers u = do
  logTVar <- gets loggedUsers
  logdUsers <- liftIO $ readTVarIO logTVar
  now <- liftIO getCurrentTime
  let logdUsers' = Map.insert (userLogin u) (now,u)
        -- filter out inactive users
        $ Map.filter ((>addUTCTime (-90*60) now).fst) logdUsers
  liftIO $ atomically $ writeTVar logTVar logdUsers'
  return logdUsers'


rmFromLoggedUsers :: AuthUser -> AppHandler ()
rmFromLoggedUsers u = do
  logdUsrs <- gets loggedUsers
  liftIO $ atomically $ modifyTVar' logdUsrs
         $ Map.delete $ userLogin u

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

errorsHandler :: AppHandler ()
errorsHandler = do
  l <- gets feLog
  r <- readRequestBody 4096
  liftIO $ withLog l $ scope "frontend" $ do
  log Info $ toStrict $ decodeUtf8 r