
module ApplicationHandlers where


import Data.Functor
import Control.Monad.IO.Class
import Control.Concurrent.STM

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8  as BU
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map

import Data.Maybe
import Data.List (foldl',sortBy)
import Data.Ord (comparing)

import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Snap
import Snap.Core
import Snap.Snaplet (with)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.Session
import Snap.Util.FileServe (serveFile)
import Snap.Util.Readable (fromBS)
------------------------------------------------------------------------------
import qualified Snaplet.DbLayer as DB
import Snaplet.FileUpload (doUpload', doDeleteAll')
------------------------------------------------------------------------------
import qualified Codec.Xlsx.Templater as Xlsx
import qualified Nominatim
------------------------------------------------------------------------------
import WeatherApi
import WeatherApi.Google
import Utils.Weather () -- instance ToJSON Weather
-----------------------------------------------------------------------------
import Application
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
      logdUsrs <- gets loggedUsers
      liftIO $ atomically $ modifyTVar' logdUsrs (Map.insert (userLogin u) ())
      avayaExt <- fromMaybe "" <$> getParam "avayaExt" >>= fromBS
      avayaPwd <- fromMaybe "" <$> getParam "avayaPwd" >>= fromBS
      with session $ do
        setInSession "avayaExt" avayaExt
        setInSession "avayaPwd" avayaPwd
        commitSession

      redirect "/"


doLogout :: AppHandler ()
doLogout = ifTop $ do
  Just u <- with auth currentUser
  logdUsrs <- gets loggedUsers
  liftIO $ atomically $ modifyTVar' logdUsrs (Map.delete $ userLogin u)
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

-----------------------------------------------------------------------------
-- | Retrieve weather
weather :: AppHandler ()
weather = ifTop $ do
  Just city     <- getParam "city"
  Right weather <- liftIO $ getWeather' (initApi "ru" "utf-8")
                                       (BU.toString city)
  writeLBS $ Aeson.encode weather

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

updateHandler :: AppHandler ()
updateHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  commit <- getJSONBody
  res <- with db $ DB.update model objId commit
  -- FIXME: try/catch & handle/log error
  writeJSON res

syncHandler :: AppHandler ()
syncHandler = do
  res <- with db DB.sync
  writeJSON res


myActionsHandler :: AppHandler ()
myActionsHandler = do
  -- FIXME : update loggedUsers
  actLock <- gets actionsLock
  do -- bracket_
    (liftIO $ atomically $ takeTMVar actLock)
    assignActions
    (liftIO $ atomically $ putTMVar actLock ())

assignActions :: AppHandler ()
assignActions = do
  Just cUsr <- with auth currentUser
  now  <- liftIO $ getCurrentTime
  let maybeEq f v a = fromMaybe False $ (==v) <$> Map.lookup f a
  acts <- filter (maybeEq "closed" "false")
       <$> with db (DB.readAll "action")
  let actsByAssignee
        = foldl'
          (\m a -> Map.insertWith' (++)
            (maybe "" T.decodeUtf8 $ Map.lookup "assignedTo" a)
            [a] m)
          Map.empty acts
  logdUsrs <- gets loggedUsers >>= liftIO . readTVarIO
  let strangersActions
        = concat $ Map.elems
        $ Map.difference actsByAssignee logdUsrs
  now <- liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
  let duetimeProximity a = case Map.lookup "duetime" a of
            Nothing -> True
            Just s -> case B.readInt s of
              Just (t, "") -> t - now < 20*60
              _ -> True
  let actionsToAssign = sortBy (comparing $ Map.lookup "priority")
        $ filter duetimeProximity strangersActions

  let assignedActions = take 1 actionsToAssign
  let updateDB a = DB.update "action" (a Map.! "id") 
                 $ Map.singleton "assignedTo" (T.encodeUtf8 $ userLogin cUsr)
  with db $ mapM_ updateDB assignedActions

  writeJSON
    $! assignedActions
    ++ Map.findWithDefault [] (userLogin cUsr) actsByAssignee
 
    
  
searchByIndex :: AppHandler ()
searchByIndex = do
  Just ixName <- getParam "indexName"
  -- FIXME: hardcoded index mockup
  case ixName of
    "allPartners" -> do
      res <- with db $ DB.readAll "partner"
      let proj obj =
            [Map.findWithDefault "" k obj
            | k <- ["id", "name", "city", "comment"]
            ]
      writeJSON $ map proj res
    "actionsForUser" -> do
      Just curUser <- with auth currentUser
      let user = T.encodeUtf8 $ userLogin curUser
      let Role userGroup = head $ userRoles curUser
      actions <- with db $ DB.readAll "action"
      let filterActions (u,g) a
            | closed /= "false" = (u,g)
            | assignedTo == user = (a:u,g)
            | assignedTo == "" && targetGroup == userGroup = (u,a:g)
            | otherwise = (u,g)
            where
              assignedTo = fromMaybe "" $ Map.lookup "assignedTo" a
              targetGroup = fromMaybe "" $ Map.lookup "targetGroup" a
              closed = fromMaybe "" $ Map.lookup "closed" a
      let (userActions,groupActions) = foldl' filterActions ([],[]) actions
      now <- liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
      let nowProximity m = case Map.lookup "duetime" m of
            Nothing -> 10^9 :: Int
            Just s -> case B.readInt s of
              Just (t, "") -> abs $ now - t
              _ -> 10^9
      let sort = sortBy (comparing nowProximity)
      writeJSON $ Map.fromList
        [("user" :: ByteString, take 30 $ sort $ userActions)
        ,("group":: ByteString, take 30 $ sort $ groupActions)]
    _ -> error $ "Unknown index " ++ show ixName

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

------------------------------------------------------------------------------
-- | Reports
report :: AppHandler ()
report = do
  Just reportId <- getParam "program"
  reportInfo <- with db $ DB.read "report" reportId
  let tplName = B.unpack (reportInfo Map.! "templates")
  let template
        = "resources/static/fileupload/report/"
        ++ (B.unpack reportId) ++ "/templates/" ++ tplName
  let result = "resources/reports/" ++ tplName
  with db $ DB.generateReport [] template result
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

------------------------------------------------------------------------------
-- | Utility functions
writeJSON :: Aeson.ToJSON v => v -> AppHandler ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v

getJSONBody :: Aeson.FromJSON v => AppHandler v
getJSONBody = Util.readJSONfromLBS <$> readRequestBody 4096

