{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Data.Configurator as Conf
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.IORef
import           Data.Maybe
import qualified Data.Map as M
import           Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Function ((&))
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Catch (throwM)
import qualified Control.Monad.Logger as MLogger
import           Control.Concurrent

import           Servant
import           Servant.Client
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Types


-- Server routes
type AppRoutes
    =  -- Search coordinates by search query.
       -- Example: GET /search/ru-RU,ru/foobarbaz
       "search" :> Capture "lang" Lang
                :> Capture "query" SearchQuery
                :> Get '[JSON] [SearchByQueryResponse]

  :<|> -- Search addresses by coordinates
       -- Example: GET /reverse-search/ru-RU,ru/52.32,3.45
       "reverse-search" :> Capture "lang" Lang
                        :> Capture "coords" Coords
                        :> Get '[JSON] SearchByCoordsResponse

  :<|> -- /debug/...
       "debug" :> (    -- GET /debug/cached-queries
                       "cached-queries" :> Get '[JSON] [()]

                  :<|> -- GET /debug/cached-responses
                       "cached-responses" :> Get '[JSON] [()]
                  )


-- Client Nominatim routes
type NominatimAPIRoutes
    =  "search" :> Header "User-Agent" UserAgent
                :> QueryParam "format" NominatimAPIFormat
                :> QueryParam "accept-language" Lang
                :> QueryParam "q" SearchQuery
                :> Get '[JSON] [SearchByQueryResponse]

  :<|> "reverse.php" :> Header "User-Agent" UserAgent
                     :> QueryParam "format" NominatimAPIFormat
                     :> QueryParam "accept-language" Lang
                     :> QueryParam "lon" NominatimLon
                     :> QueryParam "lat" NominatimLat
                     :> Get '[JSON] SearchByCoordsResponse


main :: IO ()
main = do
  cfg <- Conf.load [Conf.Required "app.cfg"]

  (port :: Warp.Port) <- Conf.require cfg "port"
  (host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"

  !(nominatimUA :: UserAgent) <-
    UserAgent <$> Conf.require cfg "nominatim.client-user-agent"

  (nominatimUrl :: String) <-
    Conf.lookupDefault "https://nominatim.openstreetmap.org" cfg "nominatim.url"

  !(nominatimBaseUrl :: BaseUrl) <- parseBaseUrl nominatimUrl
  !(manager :: Manager) <- newManager tlsManagerSettings

  loggerBus' <- newEmptyMVar
  resCache <- newIORef M.empty

  let appCtx
        = AppContext
        { responsesCache  = resCache
        , clientUserAgent = nominatimUA
        , clientEnv       = ClientEnv manager nominatimBaseUrl
        , loggerBus       = loggerBus'
        }

      app = serve (Proxy :: Proxy AppRoutes) $ appServer appCtx

      warpSettings
        = Warp.defaultSettings
        & Warp.setPort port
        & Warp.setHost (fromString host)

  -- Running logger thread
  _ <- forkIO $ MLogger.runStdoutLoggingT $ loggerInit appCtx

  logInfo appCtx
    [qmb| Nominatim config:
          \  URL: "{nominatimUrl}"
          \  Client User-Agent: "{fromUserAgent nominatimUA}" |]

  logInfo appCtx [qm| Listening on http://{host}:{port}... |]
  Warp.runSettings warpSettings app


appServer :: AppContext -> Server AppRoutes
appServer app
    =  search app
  :<|> revSearch app
  :<|> debugServer

  where
    debugServer
        =  debugCachedQueries app
      :<|> debugCachedResponses app


-- Server routes handlers

search :: AppContext -> Lang -> SearchQuery -> Handler [SearchByQueryResponse]
search appCtx lang query = do
  logInfo appCtx "Searching by query..."

  result <- liftIO $
    flip runClientM (clientEnv appCtx) $
      searchByQuery (Just $ clientUserAgent appCtx)
                    (Just NominatimJSONFormat)
                    (Just lang)
                    (Just query)

  case result of
       Left  e -> throwM e
       Right x -> pure x

revSearch :: AppContext -> Lang -> Coords -> Handler SearchByCoordsResponse
revSearch appCtx lang (Coords lon lat) = do
  logInfo appCtx "Searching by coordinates..."

  result <- liftIO $
    flip runClientM (clientEnv appCtx) $
      reverseSearchByCoords (Just $ clientUserAgent appCtx)
                            (Just NominatimJSONFormat)
                            (Just lang)
                            (Just $ NominatimLon lon)
                            (Just $ NominatimLat lat)

  case result of
       Left  e -> throwM e
       Right x -> pure x

debugCachedQueries :: AppContext -> Handler [()]
debugCachedQueries appCtx = do
  logInfo appCtx "Debugging cached queries..."
  pure []

debugCachedResponses :: AppContext -> Handler [()]
debugCachedResponses appCtx = do
  logInfo appCtx "Debugging cached responses..."
  pure []


-- Client requests to Nominatim

searchByQuery
  :: Maybe UserAgent
  -> Maybe NominatimAPIFormat
  -> Maybe Lang
  -> Maybe SearchQuery
  -> ClientM [SearchByQueryResponse]

reverseSearchByCoords
  :: Maybe UserAgent
  -> Maybe NominatimAPIFormat
  -> Maybe Lang
  -> Maybe NominatimLon
  -> Maybe NominatimLat
  -> ClientM SearchByCoordsResponse

(searchByQuery :<|> reverseSearchByCoords)
  = client (Proxy :: Proxy NominatimAPIRoutes)


-- Cleans outdated cached responses.
-- Supposed to be run in own thread.
cacheGCInit :: (LoggerBus m, MonadIO m) => AppContext -> m ()
cacheGCInit appCtx = forever $ do
  liftIO $ threadDelay cacheGCInterval
  logInfo appCtx "GC goes..."

  where
    cacheGCInterval = hour -- Every hour in microseconds
      where second = 1000 * 1000
            minute = 60 * second
            hour   = 60 * minute


-- Writes log messages somewhere.
-- Supposed to be run in own thread.
loggerInit :: (MLogger.MonadLogger m, MonadIO m) => AppContext -> m ()
loggerInit appCtx = forever $ do
  (LogMessage msgType msg) <- liftIO $ takeMVar $ loggerBus appCtx

  case msgType of
       LogInfo  -> MLogger.logInfoN  msg
       LogError -> MLogger.logErrorN msg
