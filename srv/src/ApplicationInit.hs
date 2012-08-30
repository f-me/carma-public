

module ApplicationInit (appInit) where

import Control.Monad.IO.Class

import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Data.Configurator
import Control.Concurrent.STM

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe (serveDirectory, serveFile)
------------------------------------------------------------------------------
import Snap.Snaplet.AvayaAES
import Snap.Snaplet.Vin
import Snaplet.SiteConfig
import Snaplet.DbLayer
import Snaplet.FileUpload
------------------------------------------------------------------------------
import Application
import ApplicationHandlers
----------------------------------------------------------------------
import Util (readJSON, UsersDict)




------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/",              method GET $ authOrLogin indexPage)
         , ("/login/",        method GET loginForm)
         , ("/login/",        method POST doLogin)
         , ("/logout/",       doLogout)
         , ("/nominatim",     method GET geodecode)
         , ("/s/",            serveDirectory "resources/static")
         , ("/s/screens",     serveFile "resources/site-config/screens.json")
         , ("/report",        chkAuth . method GET  $ report)
         , ("/all/:model",    chkAuth . method GET  $ readAllHandler)
         , ("/ix/callsByPhone/:phone",
            chkAuth . method GET  $ searchCallsByPhone)
         , ("/actionsFor/:id",chkAuth . method GET    $ getActionsForCase)
         , ("/myActions",     chkAuth . method GET    $ myActionsHandler)
         , ("/_whoami/",      chkAuth . method GET    $ serveUserCake)
         , ("/_/:model",      chkAuth . method POST   $ createHandler)
         , ("/_/:model/:id",  chkAuth . method GET    $ readHandler)
         , ("/_/:model/:id",  chkAuth . method PUT    $ updateHandler)
         , ("/_/:model/:id",  chkAuth . method DELETE $ deleteHandler)
         , ("/_/findOrCreate/:model/:id",
            chkAuth . method POST $ findOrCreateHandler)
         , ("/_/report/",     chkAuth . method POST $ createReportHandler)
         , ("/_/report/:id",  chkAuth . method DELETE $ deleteReportHandler)
         , ("/sync",          chkAuth . method GET  $ syncHandler)
         , ("/usersDict",     chkAuth . method GET  $ getUsersDict)
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

  s <- nestSnaplet "session" session $
       initCookieSessionManager sesKey "_session" Nothing
  authMgr <- nestSnaplet "auth" auth $
       initJsonFileAuthManager
       defAuthSettings{ asSiteKey = rmbKey
                      , asRememberPeriod = Just (rmbPer * 24 * 60 * 60)}
                               session authDb
  logdUsrs <- liftIO $ newTVarIO Map.empty
  allUsrs  <- liftIO $ getUsrs authDb
  actLock  <- liftIO $ newTMVarIO ()

  c <- nestSnaplet "cfg" siteConfig $ initSiteConfig "resources/site-config"

  d <- nestSnaplet "db" db $ initDbLayer allUsrs

  v <- nestSnaplet "vin" vin vinInit
  av <- nestSnaplet "avaya" avaya avayaAESInit
  fu <- nestSnaplet "upload" fileUpload fileUploadInit

  addRoutes routes

  return $ App h s authMgr logdUsrs allUsrs actLock c d v av fu

getUsrs authDb = do
  readJSON authDb :: IO UsersDict


------------------------------------------------------------------------------
chkAuth :: AppHandler () -> AppHandler ()
chkAuth f
  = with auth currentUser
  >>= maybe (handleError 401) (const f)


handleError :: MonadSnap m => Int -> m ()
handleError err = do
    modifyResponse $ setResponseCode err
    getResponse >>= finishWith
