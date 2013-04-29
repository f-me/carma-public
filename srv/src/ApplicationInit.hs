{-# LANGUAGE BangPatterns #-}
module ApplicationInit (appInit) where

import Control.Applicative
import Control.Monad.IO.Class

import qualified Data.Set as Set
import Data.ByteString (ByteString)
import Data.Configurator
import Control.Concurrent.STM

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
import Snap.Util.FileServe (serveDirectory, serveFile)
------------------------------------------------------------------------------
import Snap.Snaplet.Vin
import Snaplet.SiteConfig
import Snaplet.DbLayer
import Snaplet.FileUpload
import Snaplet.Geo
------------------------------------------------------------------------------
import Application
import ApplicationHandlers
import AppHandlers.ActionAssignment
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
         , ("/s/",            serveDirectory "resources/static")
         , ("/s/screens",     serveFile "resources/site-config/screens.json")
         , ("/screens",       method GET $ getScreens)
         , ("/report",        chkAuthLocal . method GET  $ report)
         , ("/all/:model",    chkAuthLocal . method GET  $ readAllHandler)
         , ("/callsByPhone/:phone",
                              chkAuthLocal . method GET    $ searchCallsByPhone)
         , ("/actionsFor/:id",chkAuthLocal . method GET    $ getActionsForCase)
         , ("/littleMoreActions",
                              chkAuthLocal . method PUT    $ littleMoreActionsHandler)
         , ("/allActions",    chkAuthLocal . method GET    $ allActionsHandler)
         , ("actions/unassigned",
                              chkAuthLocal . method GET    $ unassignedActionsHandler)
         , ("actions/busyOps",
                              chkAuthLocal . method GET    $ busyOps)
         , ("/allPartners",   chkAuthLocal . method GET    $ allPartnersHandler)
         , ("/partnersFor/:srv",
                              chkAuthLocal . method GET    $ partnersForSrvHandler)
         , ("/psaCases/",
                              chkAuthLocal . method GET    $ psaCases)
         , ("/psaCases/:program",
                              chkAuthLocal . method GET    $ psaCases)
         , ("/repTowages/:id",
                              chkAuthLocal . method GET    $ repTowages)
         , ("/allContracts/:program",
                              chkAuth . method GET   $ selectContracts)
         , ("/renderContract",
                              chkAuth . method GET    $ renderContractHandler)
         , ("/_whoami/",      chkAuth . method GET    $ serveUserCake)
         , ("/_/:model",      chkAuth . method POST   $ createHandler)
         , ("/_/:model/:id",  chkAuth . method GET    $ readHandler)
         , ("/_/:model/:id",  chkAuth . method PUT    $ updateHandler)
         , ("/_/:model/:id",  chkAuth . method DELETE $ deleteHandler)
         , ("/_/findOrCreate/:model/:id",
                              chkAuthLocal . method POST $ findOrCreateHandler)
         , ("/_/report/",     chkAuthLocal . method POST   $ createReportHandler)
         , ("/_/report/:id",  chkAuthLocal . method DELETE $ deleteReportHandler)
         , ("/searchCases",   chkAuthLocal . method GET  $ searchCases)
         , ("/latestCases",   chkAuthLocal . method GET  $ getLatestCases)
         , ("/stats/towAvgTime/:city",
            chkAuthLocal . method GET  $ towAvgTime)
         , ("/rkc",           chkAuthLocal . method GET  $ rkcHandler)
         , ("/rkc/weather",   chkAuthLocal . method GET $ rkcWeatherHandler)
         , ("/rkc/front",     chkAuthLocal . method GET $ rkcFrontHandler)
         , ("/rkc/partners",  chkAuthLocal . method GET $ rkcPartners)
         , ("/arc/:year/:month", chkAuthLocal . method GET $ arcReportHandler)
         , ("/allUsers",      chkAuth . method GET  $ serveUsersList)
         , ("/partner/upload.csv",
            chkAuthLocal . method POST $ partnerUploadData)
         , ("/vin/upload",    chkAuth . method POST $ vinUploadData)
         , ("/vin/state",     chkAuth . method GET  $ vinStateRead)
         , ("/vin/state",     chkAuth . method POST $ vinStateRemove)
         , ("/opts/:model/:id/", chkAuthLocal . method GET $ getSrvTarifOptions)
         , ("/smspost",       chkAuthLocal . method POST $ smspost)
         , ("/sms/processing", chkAuthLocal . method GET $ smsProcessingHandler)
         , ("/printSrv/:model/:id",
            chkAuthLocal . method GET $ printServiceHandler)
         , ("/runtimeFlags",  chkAuthLocal . method GET  $ getRuntimeFlags)
         , ("/runtimeFlags",  chkAuthLocal . method PUT  $ setRuntimeFlags)
         , ("/errors",        method POST errorsHandler)
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

  runtimeFlags <- liftIO $ newTVarIO Set.empty

  s <- nestSnaplet "session" session $
       initCookieSessionManager sesKey "_session" Nothing

  -- Authentication DB
  ad <- nestSnaplet "auth_db" authDb pgsInit

  authMgr <- nestSnaplet "auth" auth $ initPostgresAuth session ad

  d <- nestSnaplet "db" db $ initDbLayer authMgr authDb runtimeFlags "resources/site-config"

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
       initSiteConfig "resources/site-config" pgs db

  v <- nestSnaplet "vin" vin vinInit
  fu <- nestSnaplet "upload" fileUpload fileUploadInit
  g <- nestSnaplet "geo" geo geoInit

  l <- liftIO $ newLog (fileCfg "resources/site-config/db-log.cfg" 10)
       [logger text (file "log/frontend.log")]

  addRoutes routes
  wrapSite (claimUserActivity>>)
  return $ App h s authMgr c d pgs pga v fu g l runtimeFlags ad
