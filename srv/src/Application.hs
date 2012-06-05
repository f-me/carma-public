{-# LANGUAGE TemplateHaskell #-}

{-|

Top-level application with AuthManager and utility handlers.
Serve index page, check user login.

-}

module Application (appInit)

where

import Prelude hiding (catch, lookup)

import qualified Data.Aeson as A

import Control.Monad.IO.Class
import Data.Functor
import Data.Maybe

import Data.ByteString (ByteString)
import Data.Configurator
import Data.Lens.Template

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe

------------------------------------------------------------------------------
import Snap.Snaplet.Vin
import Snaplet.SiteConfig
import qualified Nominatim

------------------------------------------------------------------------------
-- | Application snaplet state type: Redson, Heist.
data App = App
    { _heist :: Snaplet (Heist App)
    , _session :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _siteConfig :: Snaplet (SiteConfig App)
    , _vin :: Snaplet Vin
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist


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
serveUserCake :: AuthUser -> Handler App (AuthManager App) ()
serveUserCake user = ifTop $ do
  modifyResponse $ setContentType "application/json"
  writeLBS $ A.encode user


------------------------------------------------------------------------------
-- | Geodecode mockup.
geodecode :: AppHandler ()
geodecode = ifTop $ do
  addr <- fromMaybe "Moscow" <$> getParam "addr"
  resp <- liftIO $ Nominatim.geodecode addr
  modifyResponse $ setContentType "application/json"
  writeLBS resp


withAuth
  :: (AuthUser -> Handler App (AuthManager App) ())
  -> Handler App App ()
withAuth f
  = with auth
  $ currentUser >>= maybe (handleError 401) f


handleError :: MonadSnap m => Int -> m ()
handleError err = do
    modifyResponse $ setResponseCode err
    getResponse >>= finishWith

db_create curUser = writeLBS "Db_create"
db_read   curUser = writeLBS ""
db_update curUser = writeLBS ""

report = writeLBS ""

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/",             method GET $ authOrLogin indexPage)
         , ("/login/",       method GET loginForm)
         , ("/login/",       method POST doLogin)
         , ("/logout/",      with auth $ logout >> redirectToLogin)
         , ("/nominatim",    method GET geodecode)
         , ("/s/",           serveDirectory "resources/static")
         , ("/report",       withAuth $ method POST. const report)
         , ("/_whoami/",     withAuth $ method GET . serveUserCake)
         , ("/_/:model",     withAuth $ method POST. db_create)
         , ("/_/:model/:id", withAuth $ method GET . db_read)
         , ("/_/:model/:id", withAuth $ method PUT . db_update)
         ]


sessionTimeout :: Maybe Int
sessionTimeout = Nothing


------------------------------------------------------------------------------
-- | The application initializer.
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Forms application" Nothing $ do
  cfg <- getSnapletUserConfig

  h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
  addAuthSplices auth

  sesKey <- liftIO $
            lookupDefault "resources/private/client_session_key.aes"
                          cfg "session-key"
  rmbKey <- liftIO $
            lookupDefault "resources/private/site_key.txt"
                          cfg "remember-key"
  rmbPer <- liftIO $
            lookupDefault 14
                          cfg "remember-period"
  authDb <- liftIO $
            lookupDefault "resources/private/users.json"
                          cfg "user-db"

  c <- nestSnaplet "cfg" siteConfig $ initSiteConfig auth "resources/site-config"

  s <- nestSnaplet "session" session $
       initCookieSessionManager sesKey "_session" sessionTimeout
  a <- nestSnaplet "auth" auth $
       initJsonFileAuthManager
       defAuthSettings{ asSiteKey = rmbKey
                      , asRememberPeriod = Just (rmbPer * 24 * 60 * 60)}
                               session authDb
  v <- nestSnaplet "vin" vin vinInit

  addRoutes routes

  return $ App h s a c v
