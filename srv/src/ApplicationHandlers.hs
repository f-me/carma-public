
module ApplicationHandlers where


import Data.Functor
import Control.Monad.IO.Class

import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8  as BU
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (foldl')

import Snap.Core
import Snap.Snaplet (with)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Class
import Snap.Util.FileServe (serveFile)
------------------------------------------------------------------------------
import qualified Snaplet.DbLayer as DB
import Snaplet.SiteConfig
import Snap.Snaplet.AvayaAES
------------------------------------------------------------------------------
import qualified Codec.Xlsx.Templater as Xlsx
import qualified Nominatim
------------------------------------------------------------------------------
import WeatherApi
import WeatherApi.Google
import Utils.Weather
-----------------------------------------------------------------------------
import Application


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
  r <- maybe False (const True) <$> getParam "remember"
  res <- with auth $ loginByUsername l (ClearText p) r
  case res of
    Left _err -> redirectToLogin
    Right _user -> redirect "/"


------------------------------------------------------------------------------
-- | Serve user account data back to client.
serveUserCake :: AppHandler ()
serveUserCake = ifTop
  $ withAuth currentUser
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
  Just commit <- Aeson.decode <$> getRequestBody
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
  res <- with db $ DB.readAll model (read . B.unpack <$> n)
  writeJSON res

updateHandler :: AppHandler ()
updateHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  Just commit <- Aeson.decode <$> getRequestBody
  res <- with db $ DB.update model objId commit
  -- FIXME: try/catch & handle/log error
  writeJSON res


searchByIndex :: AppHandler ()
searchByIndex = do
  Just ixName <- getParam "indexName"
  -- FIXME: hardcoded index mockup
  case ixName of
    "allPartners" -> do
      res <- with db $ DB.readAll "partner" Nothing
      let proj obj =
            [Map.findWithDefault "" k obj
            | k <- ["id", "name", "city", "comment"]
            ]
      writeJSON $ map proj res
    "actionsForUser" -> do
      Just curUser <- withAuth currentUser
      let user = T.encodeUtf8 $ userLogin curUser
      let Role userGroup = head $ userRoles curUser
      actions <- with db $ DB.readAll "action" Nothing
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
      writeJSON $ Map.fromList
        [("user" :: ByteString, take 20 $ userActions)
        ,("group":: ByteString, take 20 $ groupActions)]
    _ -> error $ "Unknown index " ++ show ixName


------------------------------------------------------------------------------
-- | Reports
report :: AppHandler ()
report = do
  liftIO $ Xlsx.run
    "resources/report-templates/all-cases.xlsx"
    "resources/static/all-cases.xlsx"
    [(Map.empty, Xlsx.TemplateSettings Xlsx.Rows 1, [])]
  serveFile "resources/static/all-cases.xlsx"


------------------------------------------------------------------------------
-- | Utility functions
writeJSON :: Aeson.ToJSON v => v -> AppHandler ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v
