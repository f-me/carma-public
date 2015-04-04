module ApplicationInit (appInit) where

import Control.Applicative
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.Configurator as Cfg

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

import WeatherApi.WWOnline (initApi)

------------------------------------------------------------------------------
import Snaplet.Avaya
import Snaplet.ChatManager
import Snaplet.SiteConfig
import qualified Snaplet.FileUpload as FU
import Snaplet.Geo
import Snaplet.Search
import Snaplet.TaskManager
import Snaplet.Messenger
------------------------------------------------------------------------------
import Application
import ApplicationHandlers
import AppHandlers.ActionAssignment
--import AppHandlers.ARC
import AppHandlers.Backoffice
import AppHandlers.Bulk
import AppHandlers.CustomSearches
import AppHandlers.PSA
import AppHandlers.RKC
import AppHandlers.ContractGenerator
import AppHandlers.Users
import AppHandlers.Screens
import AppHandlers.KPI

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
         , ("/backoffice/errors", method GET $ serveBackofficeSpec Check)
         , ("/backoffice/spec.txt", method GET $ serveBackofficeSpec Txt)
         , ("/backoffice/spec.dot", method GET $ serveBackofficeSpec Dot)
         , ("/backoffice/littleMoreActions",
            chkAuthLocal . method PUT $ littleMoreActionsHandler)
         , ("/backoffice/openAction/:actionid",
            chkAuthLocal . method PUT $ openAction)
         , ("/backoffice/caseActions/:caseid",
            chkAuthLocal . method GET $ dueCaseActions)
         , ("/backoffice/allActionResults",
            chkAuthLocal . method GET $ allActionResults)
         , ("/backoffice/allActions",
            chkAuthLocal . method GET $ allActionsHandler)
         , ("/supervisor/busyOps",  chkAuthLocal . method GET $ busyOps)
         , ("/supervisor/opStats",  chkAuthLocal . method GET $ opStats)
         , ("/supervisor/actStats", chkAuthLocal . method GET $ actStats)
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
                              chkAuthLocal . method GET $ searchContracts)
--         , ("/arcImport/:vin",
--                              chkAuthLocal . method GET $ arcImport)
         , ("/_whoami/",      chkAuth . method GET    $ serveUserCake)
         , ("/_/:model",      chkAuth . method POST   $ createHandler)
         , ("/_/:mdl",        chkAuth . method GET    $ readManyHandler)
         , ("/_/:model/:id",  chkAuth . method GET    $ readHandler)
         , ("/_/:model/:id",  chkAuth . method PUT    $ updateHandler)
         , ("/caseHistory/:caseId",
                              chkAuthLocal . method GET $ caseHistory)
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
         , ("/boUsers",       chkAuth . method GET  $ boUsers)
         , ("/dealers/:make", chkAuth . method GET  $ allDealersForMake)
         , ("/vin/upload",    chkAuth . method POST $ vinImport)
         , ("copyCtrOptions", chkAuth . method POST $ copyCtrOptions)
         , ("/clientConfig",       chkAuth . method GET  $ clientConfig)
         , ("/errors",        method POST errorsHandler)
         , ("/userStates/:userId/:from/:to",
            chkAuth . method GET $ serveUserStates)
         , ("/kpi/stat/:from/:to",      chkAuth . method GET $ getStat)
         , ("/kpi/stat/:uid/:from/:to", chkAuth . method GET $ getStat)
         , ("/kpi/statFiles/:from/:to", chkAuth . method GET $ getStatFiles)
         , ("/kpi/group/:from/:to", chkAuth . method GET $ getGroup)
         , ("/kpi/oper",           chkAuth . method GET $ getOper)
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

  wkey <- liftIO $ Cfg.lookupDefault "" cfg "weather-key"

  h <- nestSnaplet "heist" heist $ heistInit ""
  addTemplatesAt h "/" "resources/static/tpl"

  addAuthSplices h auth

  sesKey <- liftIO $
            lookupDefault "resources/private/client_session_key.aes"
                          cfg "session-key"

  s <- nestSnaplet "session" session $
       initCookieSessionManager sesKey "_session" Nothing

  -- DB
  ad <- nestSnaplet "auth_db" db pgsInit

  authMgr <- nestSnaplet "auth" auth $ initPostgresAuth session ad

  c <- nestSnaplet "cfg" siteConfig $
       initSiteConfig "resources/site-config" auth db

  fu <- nestSnaplet "upload" fileUpload $ FU.fileUploadInit db
  av <- nestSnaplet "avaya" avaya $ avayaInit auth db
  ch <- nestSnaplet "chat" chat $ chatInit auth db
  g <- nestSnaplet "geo" geo geoInit
  search' <- nestSnaplet "search" search $ searchInit authMgr db
  tm <- nestSnaplet "tasks" taskMgr $ taskManagerInit
  msgr <- nestSnaplet "wsmessenger" messenger messengerInit

  addRoutes routes
  return $ App h s authMgr c tm fu av ch g ad search' opts msgr (initApi wkey)
