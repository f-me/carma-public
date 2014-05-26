{-# LANGUAGE BangPatterns #-}
module ApplicationInit (appInit) where

import Control.Applicative
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.Configurator as Cfg

import System.Log.Simple (newLog, fileCfg, logger, text, file)

import Data.Pool
import Database.PostgreSQL.Simple as Pg

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.PostgresqlSimple (pgsInit)
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe ( serveFile
                           , simpleDirectoryConfig
                           , serveDirectoryWith
                           , DirectoryConfig(..)
                           )
------------------------------------------------------------------------------
import Snaplet.SiteConfig
import Snaplet.DbLayer
import qualified Snaplet.FileUpload as FU
import Snaplet.Geo
import Snaplet.Search
import Snaplet.TaskManager
------------------------------------------------------------------------------
import Application
import ApplicationHandlers
import AppHandlers.ActionAssignment
import AppHandlers.Bulk
import AppHandlers.CustomSearches
import AppHandlers.PSA
import AppHandlers.ContractGenerator
import AppHandlers.Users
import AppHandlers.Screens

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/",              method GET $ authOrLogin indexPage)
         , ("/login/",        method GET loginForm)
         , ("/login/",        method POST doLogin)
         , ("/logout/",       doLogout)
         , ("/s/",            serveDirectoryWith dconf "resources/static")
         , ("/s/screens",     serveFile "resources/site-config/screens.json")
         , ("/screens",       method GET $ getScreens)
         -- , ("/report",        chkAuthLocal . method GET  $ report)
         , ("/all/:model",    chkAuth . method GET  $ readAllHandler)
         , ("/callsByPhone/:phone",
                              chkAuthLocal . method GET    $ searchCallsByPhone)
         , ("/actionsFor/:id",chkAuthLocal . method GET    $ getActionsForCase)
         , ("/cancelsFor/:id",chkAuthLocal . method GET    $ getCancelsForCase)
         , ("/backoffice/littleMoreActions",
            chkAuthLocal . method PUT $ littleMoreActionsHandler)
         , ("/backoffice/openAction/:actionid",
            chkAuthLocal . method PUT $ openAction)
         , ("/backoffice/unassigned",
            chkAuthLocal . method GET $ unassignedActionsHandler)
         , ("/backoffice/allActions",
            chkAuthLocal . method GET $ allActionsHandler)
         , ("/supervisor/busyOps",  chkAuthLocal . method GET $ busyOps)
         , ("/supervisor/opStats",  chkAuthLocal . method GET $ opStats)
         , ("/supervisor/actStats", chkAuthLocal . method GET $ actStats)
         , ("/allPartners",   chkAuthLocal . method GET  $ allPartnersHandler)
         , ("/psaCases",
                              chkAuthLocal . method GET $ psaCasesHandler)
         , ("/psaCases/:program",
                              chkAuthLocal . method GET $ psaCasesHandler)
         , ("/repTowages/:id",
                              chkAuthLocal . method GET $ repTowagesHandler)
         , ("/renderContract",
                              chkAuth . method GET    $ renderContractHandler)
         , ("contracts/findSame",
                             chkAuth . method GET    $ findSameContract)
         , ("/searchContracts",
                              method GET    $ searchContracts)
         , ("/_whoami/",      chkAuth . method GET    $ serveUserCake)
         , ("/_/:model",      chkAuth . method POST   $ createHandler)
         , ("/_/:mdl",        chkAuth . method GET    $ readManyHandler)
         , ("/_/:model/:id",  chkAuth . method GET    $ readHandler)
         , ("/_/:model/:id",  chkAuth . method PUT    $ updateHandler)
         , ("/_/:model/:id",  chkAuth . method DELETE $ deleteHandler)
         , ("/_/findOrCreate/:model/:id",
                              chkAuthLocal . method POST $ findOrCreateHandler)
         , ("/_/report/",     chkAuthLocal . method POST   $ createReportHandler)
         , ("/_/report/:id",  chkAuthLocal . method DELETE $ deleteReportHandler)
         , ("/searchCases",   chkAuthLocal . method GET  $ searchCases)
         , ("/latestCases",   chkAuthLocal . method GET  $ getLatestCases)
         , ("/regionByCity/:city",
                              chkAuthLocal . method GET  $ getRegionByCity)
         , ("/stats/towAvgTime/:city",
            chkAuthLocal . method GET  $ towAvgTime)
         , ("/rkc",           chkAuthLocal . method GET  $ rkcHandler)
         , ("/rkc/weather",   chkAuthLocal . method GET $ rkcWeatherHandler)
         , ("/rkc/front",     chkAuthLocal . method GET $ rkcFrontHandler)
         , ("/rkc/partners",  chkAuthLocal . method GET $ rkcPartners)
         , ("/arc/:year/:month", chkAuthLocal . method GET $ arcReportHandler)
         , ("/allUsers",      chkAuth . method GET  $ serveUsersList)
         , ("/boUsers",       chkAuth . method GET  $ boUsers)
         , ("/dealers/:make", chkAuth . method GET  $ allDealersForMake)
         , ("/partner/upload.csv",
            chkAuthLocal . method POST $ partnerImport)
         , ("/vin/upload",    chkAuth . method POST $ vinImport)
         , ("/opts/:model/:id/", chkAuthLocal . method GET $ getSrvTarifOptions)
         , ("/smspost",       chkAuthLocal . method POST $ smspost)
         , ("/sms/processing", chkAuthLocal . method GET $ smsProcessingHandler)
         , ("/printSrv/:model/:id",
            chkAuthLocal . method GET $ printServiceHandler)
         , ("/clientConfig",       chkAuth . method GET  $ clientConfig)
         , ("/restoreProgramDefaults/:pgm",
            chkAuthAdmin . method PUT $ restoreProgramDefaults)
         , ("/errors",        method POST errorsHandler)
         , ("/userStates/:userId/:from/:to",
            chkAuth . method GET $ serveUserStates)
         ]

dconf :: DirectoryConfig (Handler App App)
dconf = simpleDirectoryConfig{preServeHook = h}
  where
    h _ = modifyResponse $ setHeader "Cache-Control" "no-cache, must-revalidate"


------------------------------------------------------------------------------
-- | The application initializer.
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Forms application" Nothing $ do
  cfg <- getSnapletUserConfig

  opts <- liftIO $ AppOptions
                <$> Cfg.lookup cfg "local-name"
                <*> Cfg.lookupDefault 4 cfg "search-min-length"

  h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
  addAuthSplices h auth

  sesKey <- liftIO $
            lookupDefault "resources/private/client_session_key.aes"
                          cfg "session-key"

  s <- nestSnaplet "session" session $
       initCookieSessionManager sesKey "_session" Nothing

  -- Authentication DB
  ad <- nestSnaplet "auth_db" authDb pgsInit

  authMgr <- nestSnaplet "auth" auth $ initPostgresAuth session ad

  d <- nestSnaplet "db" db $ initDbLayer authMgr authDb "resources/site-config"

  -- init PostgreSQL connection pool that will be used for searching only
  let lookupCfg nm = lookupDefault (error $ show nm) cfg nm
  cInfo <- liftIO $ Pg.ConnectInfo
            <$> lookupCfg "pg_host"
            <*> lookupCfg "pg_port"
            <*> lookupCfg "pg_search_user"
            <*> lookupCfg "pg_search_pass"
            <*> lookupCfg "pg_db_name"
  -- FIXME: force cInfo evaluation
  pgs <- liftIO $ createPool (Pg.connect cInfo) Pg.close 1 5 20
  cInfoActass <- liftIO $ (\u p -> cInfo {connectUser = u, connectPassword = p})
            <$> lookupCfg "pg_actass_user"
            <*> lookupCfg "pg_actass_pass"
  pga <- liftIO $ createPool (Pg.connect cInfoActass) Pg.close 1 5 20

  c <- nestSnaplet "cfg" siteConfig $
       initSiteConfig "resources/site-config" pgs

  fu <- nestSnaplet "upload" fileUpload $ FU.fileUploadInit db
  g <- nestSnaplet "geo" geo geoInit

  l <- liftIO $ newLog (fileCfg "resources/site-config/db-log.cfg" 10)
       [logger text (file "log/frontend.log")]

  search' <- nestSnaplet "search" search $ searchInit pgs authMgr db
  tm <- nestSnaplet "tasks" taskMgr $ taskManagerInit
  addRoutes routes
  wrapSite (claimUserActivity>>)
  return $ App h s authMgr c d pgs pga tm fu g l ad search' opts
