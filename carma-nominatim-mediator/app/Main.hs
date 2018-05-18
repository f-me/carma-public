{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Data.Configurator as Conf
import           Data.String (fromString)
import           Data.IORef
import qualified Data.Map as M
import           Data.Proxy
import           Data.Function ((&))
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.Catch (MonadThrow, throwM, toException)
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent

import           Servant
import           Servant.Client
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Logger
import           Carma.NominatimMediator.CacheGC
import           Carma.NominatimMediator.Utils


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
  requestExecutorBus' <- newEmptyMVar
  resCache <- newIORef M.empty

  let appCtx
        = AppContext
        { responsesCache     = resCache
        , clientUserAgent    = nominatimUA
        , clientEnv          = ClientEnv manager nominatimBaseUrl
        , loggerBus          = loggerBus'
        , requestExecutorBus = requestExecutorBus'
        }

      app = serve (Proxy :: Proxy AppRoutes) $ appServer appCtx

      warpSettings
        = Warp.defaultSettings
        & Warp.setPort port
        & Warp.setHost (fromString host)

  -- Running logger thread
  _ <- forkIO $ runStdoutLoggingT $ loggerInit appCtx

  -- Running cache garbage collector thread
  -- which cleans outdated cached responses.
  _ <- forkIO $ cacheGCInit appCtx

  -- Running requests queue handler
  _ <- forkIO $ requestExecutorInit appCtx

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
  let reqParams = SearchQueryReq lang query

      req = SearchByQueryResponse' <$>
            searchByQuery (Just $ clientUserAgent appCtx)
                          (Just NominatimJSONFormat)
                          (Just lang)
                          (Just query)

  logInfo appCtx [qm| Searching by query with params: {reqParams}... |]
  result <- requestResponse appCtx reqParams req

  case result of
       SearchByQueryResponse' x -> pure x
       x -> throwUnexpectedResponse appCtx x

revSearch :: AppContext -> Lang -> Coords -> Handler SearchByCoordsResponse
revSearch appCtx lang coords@(Coords lon' lat') = do
  let reqParams = RevSearchQueryReq lang coords

      req = SearchByCoordsResponse' <$>
            reverseSearchByCoords (Just $ clientUserAgent appCtx)
                                  (Just NominatimJSONFormat)
                                  (Just lang)
                                  (Just $ NominatimLon lon')
                                  (Just $ NominatimLat lat')

  logInfo appCtx [qm| Searching by coordinates with params: {reqParams}... |]
  result <- requestResponse appCtx reqParams req

  case result of
       SearchByCoordsResponse' x -> pure x
       x -> throwUnexpectedResponse appCtx x

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


-- Throwing `UnexpectedResponseResultException` with logging this error
throwUnexpectedResponse
  :: (MonadThrow m, LoggerBus m) => AppContext -> Response -> m a
throwUnexpectedResponse appCtx x = do
  logError appCtx [qm| Unexpected response result: {x}! |]
  throwM $ UnexpectedResponseResultException x


-- Requests queue.
-- Supposed to be run in own thread.
-- It writes response to provided `MVar`.
requestExecutorInit :: (LoggerBus m, MonadIO m) => AppContext -> m ()
requestExecutorInit appCtx = do
  logInfo appCtx [qms| Running request executor,
                       waiting for {intervalBetweenRequestsInSeconds} seconds
                       before start in case application restarted quickly just
                       after previous request... |]

  liftIO $ threadDelay intervalBetweenRequests
  logInfo appCtx [qn| Request executor is ready and waiting for requests... |]

  forever $ do
    (reqParams, req, responseBus) <-
      liftIO $ takeMVar $ requestExecutorBus appCtx

    logInfo appCtx [qm| Executing request with params: {reqParams}... |]
    result <- liftIO $ runClientM req $ clientEnv appCtx

    case result of
         Left e -> logInfo appCtx [qms| Request by params {reqParams}
                                        is failed with exception: {e}. |]
         _ -> pure ()

    liftIO $ putMVar responseBus result

    logInfo appCtx [qms| Request executor will wait for
                         {intervalBetweenRequestsInSeconds} seconds
                         before handling next request... |]

    liftIO $ threadDelay intervalBetweenRequests

  where
    -- Minimum interval is one second, making it little more safe
    intervalBetweenRequests = round $ secondInMicroseconds * 1.5

    intervalBetweenRequestsInSeconds =
      fromIntegral intervalBetweenRequests / secondInMicroseconds


-- Sends request to requests executor bus
-- and creates response bus and waits for response.
requestResponse :: (MonadIO m, MonadThrow m, LoggerBus m)
                => AppContext
                -> RequestParams
                -> ClientM Response
                -> m Response

requestResponse appCtx reqParams req = do
  responseBus <- liftIO newEmptyMVar

  -- Handling timeout case
  thread <- liftIO $ forkIO $ do
    threadDelay timeout
    putMVar responseBus $ Left $
      ConnectionError $ toException $ RequestTimeoutException reqParams

  liftIO $ requestExecutorBus appCtx `putMVar` (reqParams, req, responseBus)
  result <- liftIO $ takeMVar responseBus
  liftIO $ killThread thread -- No need for timeout thread anymore

  case result of
       Left  e -> throwM e
       Right x -> pure x

  where timeout = round $ 15 * secondInMicroseconds
