{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import qualified Data.Configurator as Conf
import           Data.String (fromString)
import           Data.IORef
import qualified Data.Map as M
import           Data.Aeson (toJSON)
import           Data.Proxy
import           Data.Function ((&))
import           Text.InterpolatedString.QM

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
import           Carma.NominatimMediator.RequestExecutor


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
                       "cached-queries" :> Get '[JSON] [DebugCachedQuery]

                  :<|> -- GET /debug/cached-responses
                       "cached-responses" :> Get '[JSON] [DebugCachedResponse]
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

  resCache            <- newIORef M.empty
  loggerBus'          <- newEmptyMVar
  requestExecutorBus' <- newEmptyMVar

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

debugCachedQueries :: AppContext -> Handler [DebugCachedQuery]
debugCachedQueries appCtx = do
  logInfo appCtx "Debugging cached queries..."
  liftIO $ readIORef (responsesCache appCtx) <&> M.foldrWithKey reducer []
  where reducer k (t, _) = (DebugCachedQuery k [qm| {formatTime t} UTC |] :)

debugCachedResponses :: AppContext -> Handler [DebugCachedResponse]
debugCachedResponses appCtx = do
  logInfo appCtx "Debugging cached responses..."
  liftIO $ readIORef (responsesCache appCtx) <&> M.foldrWithKey reducer []

  where
    reducer k (t, response') acc
      = DebugCachedResponse
      { request_params = k
      , time           = [qm| {formatTime t} UTC |]
      , response_type  = rtype
      , response       = rjson
      } : acc
      where (rtype, rjson) =
              case response' of
                   SearchByQueryResponse'  x -> ("search",         toJSON x)
                   SearchByCoordsResponse' x -> ("reverse-search", toJSON x)


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

  where timeout = round $ 15 * secondInMicroseconds :: Int
