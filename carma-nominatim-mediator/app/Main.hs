-- A microservice which handles requests to Nominatim.
-- It controls intervals between requests (to prevent exceeding the limits of
-- shared community Nominatim server) and caches responses.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Data.Configurator as Conf
import           Data.String (fromString)
import qualified Data.Map as M
import           Data.Aeson (toJSON)
import           Data.Proxy
import           Data.List (sortBy)
import           Data.Function ((&), on)
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.Catch (MonadThrow, throwM, toException)
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent

import           System.Directory (makeAbsolute)

import           Servant
import           Servant.Client
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Logger
import           Carma.NominatimMediator.CacheGC
import           Carma.NominatimMediator.CacheSync
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

  !(port :: Warp.Port) <- Conf.require cfg "port"
  !(host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"

  !(cacheFile :: Maybe FilePath) <-
    Conf.lookup cfg "cache.synchronizer.snapshot-file" >>=
      \case Nothing -> pure Nothing
            Just x  -> Just <$!> makeAbsolute x

  !(syncInterval   :: Float) <- Conf.require cfg "cache.synchronizer.interval"
  !(gcInterval     :: Float) <- Conf.require cfg "cache.gc.interval"
  !(cachedLifetime :: Float) <- Conf.require cfg "cache.gc.lifetime"

  !(nominatimUA :: UserAgent) <-
    UserAgent <$> Conf.require cfg "nominatim.client-user-agent"

  !(nominatimUrl :: String) <-
    Conf.lookupDefault "https://nominatim.openstreetmap.org" cfg "nominatim.url"

  !(nominatimReqGap :: Float) <-
    Conf.require cfg "nominatim.gap-between-requests"

  !(nominatimBaseUrl :: BaseUrl) <- parseBaseUrl nominatimUrl
  !(manager          :: Manager) <- newManager tlsManagerSettings

  resCache            <- newIORefWithCounter M.empty
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

  -- Trying to fill cache with initial snapshot
  maybe (pure ()) (fillCacheWithSnapshot appCtx) cacheFile

  -- Running cache garbage collector thread
  -- which cleans outdated cached responses.
  _ <- forkIO $ cacheGCInit appCtx gcInterval cachedLifetime

  -- Syncing with file is optional,
  -- if you don't wanna this feature
  -- just remove "cache-file" from config.
  -- Running cache synchronizer thread
  -- which stores cache snapshot to a file
  -- to be able to load it after restart.
  maybe (logInfo appCtx [qms| Cache file to save snapshots to isn't set,
                              cache synchronizer feature is disabled. |])
        (void . forkIO . cacheSyncInit appCtx syncInterval)
        cacheFile

  -- Running requests queue handler
  _ <- forkIO $ requestExecutorInit appCtx nominatimReqGap

  logInfo appCtx
    [qmb| Subsystems is initialized.
          GC config:
          \  Checks interval: {gcInterval} hour(s)
          \  Cached response lifetime: {cachedLifetime} hour(s)
          Nominatim config:
          \  URL: "{nominatimUrl}"
          \  Client User-Agent: "{fromUserAgent nominatimUA}"
          \  Gap between requests: {nominatimReqGap} second(s) |]

  logInfo appCtx [qm| Listening on http://{host}:{port}... |]
  Warp.runSettings warpSettings app


appServer :: AppContext -> Server AppRoutes
appServer app
    =  search app
  :<|> revSearch app
  :<|> (    debugCachedQueries app
       :<|> debugCachedResponses app
       )


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
  readIORefWithCounter (responsesCache appCtx) <&>
    M.assocs ? sortBy (compare `on` snd ? fst) ? foldl reducer []

  where reducer acc (k, (t, _)) =
          DebugCachedQuery k [qm| {formatTime t} UTC |] : acc

debugCachedResponses :: AppContext -> Handler [DebugCachedResponse]
debugCachedResponses appCtx = do
  logInfo appCtx "Debugging cached responses..."
  readIORefWithCounter (responsesCache appCtx) <&>
    M.assocs ? sortBy (compare `on` snd ? fst) ? foldl reducer []

  where
    reducer acc (k, (t, response'))
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
