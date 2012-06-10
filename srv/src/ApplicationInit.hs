

module ApplicationInit (appInit) where

import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.Configurator

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe (serveDirectory)
------------------------------------------------------------------------------
import Database.Redis (defaultConnectInfo)
import Snap.Snaplet.RedisDB (redisDBInit)

import Snap.Snaplet.AvayaAES
import Snap.Snaplet.Vin
import Snaplet.SiteConfig
------------------------------------------------------------------------------
import Application
import ApplicationHandlers




------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/",             method GET $ authOrLogin indexPage)
         , ("/login/",       method GET loginForm)
         , ("/login/",       method POST doLogin)
         , ("/logout/",      with auth $ logout >> redirectToLogin)
         , ("/nominatim",    method GET geodecode)
         , ("/s/",           serveDirectory "resources/static")
         , ("/report",       withAuth $ method GET . const report)
         , ("/_whoami/",     withAuth $ method GET . serveUserCake)
         , ("/_/:model",     withAuth $ method POST. createHandler)
         , ("/_/:model/:id", withAuth $ method GET . readHandler)
         , ("/_/:model/:id", withAuth $ method PUT . updateHandler)
         ]


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
       initCookieSessionManager sesKey "_session" Nothing
  a <- nestSnaplet "auth" auth $
       initJsonFileAuthManager
       defAuthSettings{ asSiteKey = rmbKey
                      , asRememberPeriod = Just (rmbPer * 24 * 60 * 60)}
                               session authDb
  r <- nestSnaplet "db" redis $ redisDBInit defaultConnectInfo

  v <- nestSnaplet "vin" vin vinInit
  av <- nestSnaplet "avaya" avaya $ avayaAESInit auth

  addRoutes routes

  return $ App h s a c r v av


------------------------------------------------------------------------------
withAuth :: (AuthUser -> AppHandler ()) -> AppHandler ()
withAuth f
  = with auth currentUser
  >>= maybe (handleError 401) f


handleError :: MonadSnap m => Int -> m ()
handleError err = do
    modifyResponse $ setResponseCode err
    getResponse >>= finishWith
