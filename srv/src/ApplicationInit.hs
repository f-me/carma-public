module ApplicationInit (appInit) where

import           Control.Concurrent.STM
import           Control.Exception.Lifted                    hiding (Handler)
import           Control.Monad                               (when)
import           Control.Monad.IO.Class

import           Data.ByteString                             (ByteString)
import           Data.Configurator                           as Cfg
import qualified Data.Map                                    as Map
import qualified Data.Text.Encoding                          as T
import qualified Data.Time.Clock                             as Clock

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth                           hiding (session)
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.PostgresqlSimple               (pgsInit)
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe                         (DirectoryConfig (..),
                                                              serveDirectoryWith,
                                                              serveFile,
                                                              simpleDirectoryConfig)

import           WeatherApi.OpenWeatherMap                   (initApi)

------------------------------------------------------------------------------
import           Snaplet.ChatManager
import qualified Snaplet.FileUpload                          as FU
import           Snaplet.Geo
import           Snaplet.Messenger
import           Snaplet.Search
import           Snaplet.SiteConfig
import           Snaplet.TaskManager
------------------------------------------------------------------------------
import           AppHandlers.ActionAssignment
import           AppHandlers.Avaya
import           Application
import           ApplicationHandlers
--import AppHandlers.ARC
import           AppHandlers.Backoffice
import           AppHandlers.Bulk
import           AppHandlers.ContractGenerator
import           AppHandlers.CustomSearches
import           AppHandlers.DiagTree
import           AppHandlers.KPI
import           AppHandlers.PSA
import           AppHandlers.RKC
import           AppHandlers.Screens
import           AppHandlers.Users
import           Util

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/",              method GET $ authOrLogin indexPage)
         , ("/pure/",         method GET $ authOrLogin purePage)
         , ("/login/",        method GET loginForm)
         , ("/login/",        method POST doLogin)
         , ("/logout/",       doLogout)
         , ("/s/",            serveDirectoryWith dconf "resources/static")
         , ("/s/frontend",    serveDirectoryWith dconf "resources/static/build/frontend")
         , ("/s/pureFrontend", serveDirectoryWith dconf "resources/static/build/pureFrontend")
         , ("/s/screens",     serveFile "resources/site-config/screens.json")
         , ("/screens",       method GET getScreens)
         , ("/backoffice/errors", method GET $ serveBackofficeSpec Check)
         , ("/backoffice/spec.txt", method GET $ serveBackofficeSpec Txt)
         , ("/backoffice/spec.dot", method GET $ serveBackofficeSpec Dot)
         , ("/backoffice/myActions",
            chkAuthLocal . method GET $ myActions)
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
         , ("/backoffice/suspendedServices",
            chkAuthLocal . method GET $ suspendedServices)
         , ("/backoffice/abandonedServices/:usr",
            chkAuthLocal . method GET $ abandonedServices)
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
         , ("/contracts/copy/:id",
                              chkAuth . method POST   $ copyContract)
         , ("contracts/findSame",
                              chkAuth . method GET    $ findSameContract)
         , ("/searchContracts",
                              chkAuthLocal . method GET $ searchContracts)
         , ("/findContractByPhone/:phone",
                              chkAuthLocal . method GET $ findContractByPhone)
--         , ("/arcImport/:vin",
--                              chkAuthLocal . method GET $ arcImport)
         , ("/_whoami/",      chkAuth . method GET    $ serveUserCake)
         , ("/_/:model",      chkAuth . method POST   $ createHandler)
         , ("/_/:mdl",        chkAuth . method GET    $ readManyHandler)
         , ("/_/:model/:id",  chkAuth . method GET    $ readHandler)
         , ("/_/:model/:id",  chkAuth . method PUT    $ updateHandler)
         , ("/partnerKPI/:svcid/:partnerid",
                              chkAuth . method GET    $ partnerKPI)
         , ("/caseHistory/:caseId",
                              chkAuthLocal . method GET $ caseHistory)
         , ("/relevantCases/:caseId",
                              chkAuthLocal . method GET  $ relevantCases)
         , ("/searchCases",   chkAuthLocal . method GET  $ searchCases)
         , ("/latestCases",   chkAuthLocal . method GET  $ getLatestCases)
         , ("/regionByCity/:city",
                              chkAuthLocal . method GET  $ getRegionByCity)
         , ("/stats/towAvgTime/:city",
            chkAuthLocal . method GET  $ towAvgTime)
         , ("/rkc",           chkAuthLocal . method GET  $ rkcHandler)
         , ("/rkc/weather",   chkAuthLocal . method GET $ rkcWeatherHandler)
         , ("/rkc/partners",  chkAuthLocal . method GET $ rkcPartners)
         , ("/boUsers",       chkAuth . method GET  $ boUsers)
         , ("/loggedUsers",   chkAuth . method GET  $ loggedUsers)
         , ("/dealers/:make", chkAuth . method GET  $ allDealersForMake)
         , ("/vin/upload",    chkAuth . method POST $ vinImport)
         , ("/vin/importDirectory",
                              chkAuth . method GET $ vinImportDirectory)
         , ("copyCtrOptions", chkAuth . method POST $ copyCtrOptions)
         , ("/clientConfig",       chkAuth . method GET  $ clientConfig)
         , ("/whoopsie",      chkAuth . method POST $ whoopsieHandler)
         , ("/userStates/:userId/:from/:to",
            chkAuth . method GET $ serveUserStates)
         , ("/kpi/stat/:from/:to",      chkAuth . method GET $ getStat)
         , ("/kpi/stat/:uid/:from/:to", chkAuth . method GET $ getStat)
         , ("/kpi/statFiles/:from/:to", chkAuth . method GET $ getStatFiles)
         , ("/kpi/group/:from/:to",     chkAuth . method GET $ getGroup)
         , ("/kpi/oper",                chkAuth . method GET $ getOper)
         , ("/avaya/eject/:user/:call", chkAuth . method PUT $ ejectUser)
         , ("/avaya/ws/:ext",           chkAuth . method GET $ dmccWsProxy)
         , ("/avaya/hook/",             chkAuth . method POST $ dmccHook)
         , ("/avaya/toAfterCall/",      chkAuth . method PUT $ avayaToAfterCall)
         , ("/avaya/toReady/",          chkAuth . method PUT $ avayaToReady)
         , ("/diag/info/:caseId",       chkAuth . method GET $ diagInfo)
         , ("/diag/history/:caseId",    chkAuth . method GET $ diagHistory)
         , ("/diag/retry/:histId",      chkAuth . method POST $ retryQuestion)
         , ("/diag/slide/move/",        chkAuth . method POST $
                                          moveOrCopyDiagSlide MoveDiagSlide)
         , ("/diag/slide/copy/",        chkAuth . method POST $
                                          moveOrCopyDiagSlide CopyDiagSlide)
         , ("/meta",                    method GET serveMeta)
         ]

dconf :: DirectoryConfig (Handler App App)
dconf = simpleDirectoryConfig{preServeHook = h}
  where
    h _ = modifyResponse $ setHeader "Cache-Control" "no-cache, must-revalidate"


timeIt :: AppHandler () -> AppHandler ()
timeIt h = do
  r <- getRequest
  start <- liftIO Clock.getCurrentTime
  finally h $ liftIO $ do
    end <- Clock.getCurrentTime
    let duration = Clock.diffUTCTime end start
    when (1 <= duration)
      $ syslogJSON Info "timeIt"
        [ "method" .= show (rqMethod r)
        , "uri" .= T.decodeUtf8 (rqURI r)
        , "time" .= show duration
        ]

------------------------------------------------------------------------------
-- | The application initializer.
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Forms application" Nothing $ do
  cfg <- getSnapletUserConfig

  opts <- liftIO $ AppOptions
                <$> Cfg.lookup cfg "local-name"
                <*> Cfg.lookupDefault 4 cfg "search-min-length"
                <*> Cfg.lookupDefault "localhost" cfg "dmcc-ws-host"
                <*> Cfg.lookupDefault 8333 cfg "dmcc-ws-port"

  wkey <- liftIO $ Cfg.lookupDefault "" cfg "weather-key"

  h <- nestSnaplet "heist" heist $ heistInit ""
  addTemplatesAt h "/" "resources/static/build/backendPages"

  addAuthSplices h auth

  sesKey <- liftIO $
            lookupDefault "resources/private/client_session_key.aes"
                          cfg "session-key"

  s <- nestSnaplet "session" session $
       let lifetime = Just $ 365 * 24 * 60 * 60 -- One year in seconds
        in initCookieSessionManager sesKey "_session" Nothing lifetime

  -- DB
  ad <- nestSnaplet "db" db pgsInit

  ad2 <- nestSnaplet "db2" db2 $ initPersist $ return ()

  authMgr <- nestSnaplet "auth" auth $ initPostgresAuth session ad

  c <- nestSnaplet "cfg" siteConfig $
       initSiteConfig "resources/site-config" auth db

  fu <- nestSnaplet "upload" fileUpload $ FU.fileUploadInit db
  ch <- nestSnaplet "chat" chat $ chatInit auth db
  g <- nestSnaplet "geo" geo $ geoInit db
  search' <- nestSnaplet "search" search $ searchInit auth db
  tm <- nestSnaplet "tasks" taskMgr taskManagerInit
  msgr <- nestSnaplet "wsmessenger" messenger messengerInit

  addRoutes [(n, timeIt f) | (n, f) <- routes]

  em <- liftIO $ newTVarIO Map.empty

  pure $ App h s authMgr c tm fu ch g ad ad2 search' opts msgr (initApi wkey) em
