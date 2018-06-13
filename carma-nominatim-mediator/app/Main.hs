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
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Data.Configurator as Conf
import           Data.String (fromString)
import qualified Data.Map as M
import           Data.Aeson (toJSON)
import           Data.Proxy
import           Data.List (sortBy)
import           Data.Function ((&), on)
import           Text.InterpolatedString.QM
import           Data.Swagger (Swagger)

import           Control.Monad
import           Control.Monad.Catch (MonadThrow, throwM, toException)
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.Reader.Class (MonadReader, reader)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)

import           System.Directory (makeAbsolute)

import           Servant
import           Servant.Client
import           Servant.Swagger (toSwagger)
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
  =    -- Search coordinates by search query.
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

type SwaggerHeaders a
   = Headers '[Header "Access-Control-Allow-Origin" String] a

-- Server routes + swagger debug route
type AppRoutesWithSwagger
  =    AppRoutes
  :<|> -- GET /debug/swagger.json
       "debug" :> "swagger.json" :> Get '[JSON] (SwaggerHeaders Swagger)


-- Client Nominatim routes
type NominatimAPIRoutes
  =    "search" :> Header "User-Agent" UserAgent
                :> QueryParam "format" NominatimAPIFormat
                :> QueryParam "accept-language" Lang
                :> QueryParam "q" SearchQuery
                :> Get '[JSON] [SearchByQueryResponse]

  :<|> "reverse" :> Header "User-Agent" UserAgent
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

  !(noCacheForRevSearch :: Bool) <-
    Conf.lookupDefault True cfg "cache.disable-cache-for-reverse-search"

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
        { responsesCache              = resCache
        , clientUserAgent             = nominatimUA
        , clientEnv                   = ClientEnv manager nominatimBaseUrl
        , cacheForRevSearchIsDisabled = noCacheForRevSearch
        , loggerBus                   = loggerBus'
        , requestExecutorBus          = requestExecutorBus'
        }

      app = serve (Proxy :: Proxy AppRoutesWithSwagger) $ appServer appCtx

      warpSettings
        = Warp.defaultSettings
        & Warp.setPort port
        & Warp.setHost (fromString host)

  flip runReaderT appCtx $ do

    -- Running logger thread
    _ <- fork $ runStdoutLoggingT $ loggerInit

    -- Trying to fill cache with initial snapshot
    maybe (pure ()) fillCacheWithSnapshot cacheFile

    -- Running cache garbage collector thread
    -- which cleans outdated cached responses.
    _ <- fork $ cacheGCInit gcInterval cachedLifetime

    -- Syncing with file is optional,
    -- if you don't wanna this feature
    -- just remove "cache-file" from config.
    -- Running cache synchronizer thread
    -- which stores cache snapshot to a file
    -- to be able to load it after restart.
    case cacheFile of
         Just x  -> void $ fork $ cacheSyncInit syncInterval x
         Nothing -> logInfo [qms| Cache file to save snapshots to isn't set,
                                  cache synchronizer feature is disabled. |]

    -- Running requests queue handler
    _ <- fork $ requestExecutorInit nominatimReqGap

    logInfo
      [qmb| Subsystems is initialized.
            GC config:
            \  Checks interval: {gcInterval} hour(s)
            \  Cached response lifetime: {cachedLifetime} hour(s)
            Nominatim config:
            \  URL: "{nominatimUrl}"
            \  Client User-Agent: "{fromUserAgent nominatimUA}"
            \  Gap between requests: {nominatimReqGap} second(s) |]

    logInfo [qm| Listening on http://{host}:{port}... |]

  Warp.runSettings warpSettings app


appServer :: AppContext -> Server AppRoutesWithSwagger
appServer appCtx =
  ( (\lang query -> wrap $ search lang query)
    :<|> (\lang coords -> wrap $ revSearch lang coords)
    :<|> ( wrap debugCachedQueries
           :<|> wrap debugCachedResponses
         )
  )
  :<|> wrap debugSwaggerAPI

  where wrap = flip runReaderT appCtx


-- Server routes handlers

search
  :: ( MonadReader AppContext m
     , MonadThrow m
     , ThreadMonad m
     , DelayMonad m
     , MVarMonad m
     , MonadIO m
     )
  => Lang -> SearchQuery -> m [SearchByQueryResponse]
search lang query = do
  let reqParams = SearchQueryReq lang query
  clientUserAgent' <- reader clientUserAgent

  let req = SearchByQueryResponse' <$>
            searchByQuery (Just clientUserAgent')
                          (Just NominatimJSONFormat)
                          (Just lang)
                          (Just query)

  logInfo [qm| Searching by query with params: {reqParams}... |]
  result <- requestResponse reqParams req

  case result of
       SearchByQueryResponse' x -> pure x
       x -> throwUnexpectedResponse x

revSearch
  :: ( MonadReader AppContext m
     , MonadThrow m
     , ThreadMonad m
     , DelayMonad m
     , MVarMonad m
     , MonadIO m
     )
  => Lang -> Coords -> m SearchByCoordsResponse
revSearch lang coords@(Coords lon' lat') = do
  let reqParams = RevSearchQueryReq lang coords
  clientUserAgent' <- reader clientUserAgent

  let req = SearchByCoordsResponse' <$>
            reverseSearchByCoords (Just clientUserAgent')
                                  (Just NominatimJSONFormat)
                                  (Just lang)
                                  (Just $ NominatimLon lon')
                                  (Just $ NominatimLat lat')

  logInfo [qm| Searching by coordinates with params: {reqParams}... |]
  result <- requestResponse reqParams req

  case result of
       SearchByCoordsResponse' x -> pure x
       x -> throwUnexpectedResponse x

debugCachedQueries
  :: ( MonadReader AppContext m
     , LoggerBusMonad m
     , IORefWithCounterMonad m
     )
  => m [DebugCachedQuery]
debugCachedQueries = do
  logInfo "Debugging cached queries..."

  (reader responsesCache >>= readIORefWithCounter)
    <&> M.assocs ? sortBy (compare `on` snd ? fst) ? foldl reducer []

  where reducer acc (k, (t, _)) =
          DebugCachedQuery k [qm| {formatTime t} UTC |] : acc

debugCachedResponses
  :: ( MonadReader AppContext m
     , LoggerBusMonad m
     , IORefWithCounterMonad m
     )
  => m [DebugCachedResponse]
debugCachedResponses = do
  logInfo "Debugging cached responses..."

  (reader responsesCache >>= readIORefWithCounter)
    <&> M.assocs ? sortBy (compare `on` snd ? fst) ? foldl reducer []

  where
    reducer acc (k, (t, response'))
      = DebugCachedResponse
      { request_params = k
      , time           = [qm| {formatTime t} UTC |]
      , response_type  = requestType response'
      , response       = rjson
      } : acc
      where rjson = case response' of
                         SearchByQueryResponse'  x -> toJSON x
                         SearchByCoordsResponse' x -> toJSON x

-- Allowing cross-origin requests to be able to use online swagger-codegen.
debugSwaggerAPI
  :: Applicative m
  => m (Headers '[Header "Access-Control-Allow-Origin" String] Swagger)
debugSwaggerAPI = pure $ addHeader "*" $ toSwagger (Proxy :: Proxy AppRoutes)


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
  :: (MonadThrow m, MonadReader AppContext m, LoggerBusMonad m)
  => Response
  -> m a
throwUnexpectedResponse x = do
  logError [qm| Unexpected response result: {x}! |]
  throwM $ UnexpectedResponseResultException x


-- Sends request to requests executor bus
-- and creates response bus and waits for response.
requestResponse
  :: ( MonadReader AppContext m
     , LoggerBusMonad m
     , MonadThrow m
     , ThreadMonad m
     , DelayMonad m
     , MVarMonad m
     )
  => RequestParams
  -> ClientM Response
  -> m Response
requestResponse reqParams req = do
  responseBus <- newEmptyMVar

  -- Handling timeout case
  thread <- fork $ do
    delay timeout
    putMVar responseBus $ Left $
      ConnectionError $ toException $ RequestTimeoutException reqParams

  reader requestExecutorBus >>=
    flip putMVar (reqParams, req, responseBus)

  result <- takeMVar responseBus
  killThread thread -- No need for timeout thread anymore

  case result of
       Left  e -> throwM e
       Right x -> pure x

  where timeout = round $ 15 * secondInMicroseconds :: Int
