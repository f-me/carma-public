
module ApplicationHandlers where


import Data.Functor
import Control.Monad.IO.Class

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

import Snap.Core
import Snap.Snaplet (with)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.Session
import Snap.Util.FileServe (serveFile)
import Snap.Util.Readable (fromBS)
------------------------------------------------------------------------------
import qualified Snaplet.DbLayer as DB
import Snaplet.FileUpload (doUpload')
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

  avayaExt <- fromMaybe "" <$> getParam "avayaExt" >>= fromBS
  avayaPwd <- fromMaybe "" <$> getParam "avayaPwd" >>= fromBS
  with session $ do
    setInSession "avayaExt" avayaExt
    setInSession "avayaPwd" avayaPwd
    commitSession

  either (const redirectToLogin) (const $ redirect "/") res


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
  n <- getParam "limit"
  res <- with db $ DB.readAll model
  let proj obj = Map.fromList
        [(k, Map.findWithDefault "" k obj)
        | k <- ["id", "caller_name", "callDate", "caller_phone1"
               ,"car_plateNum", "car_vin", "program", "comment"
               ,"name", "templates"]
        ]
  let res' = map proj $ sortBy (flip $ comparing $ Map.lookup "callDate") res
  writeJSON $ maybe res' (`take` res') (read . B.unpack <$> n)

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

------------------------------------------------------------------------------
-- | Reports
report :: AppHandler ()
report = do
  liftIO $ Xlsx.run
    "resources/report-templates/all-cases.xlsx"
    "resources/static/all-cases.xlsx"
    [(Map.empty, Xlsx.TemplateSettings Xlsx.Rows 1, [])]
  modifyResponse $ addHeader "Content-Disposition" "attachment; filename=\"report.xlsx\""
  serveFile "resources/static/all-cases.xlsx"

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
  writeLBS "ololo"

------------------------------------------------------------------------------
-- | Utility functions
writeJSON :: Aeson.ToJSON v => v -> AppHandler ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v

getJSONBody :: Aeson.FromJSON v => AppHandler v
getJSONBody = Util.readJSONfromLBS <$> readRequestBody 4096

