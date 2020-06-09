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
import qualified Data.HashMap.Strict as HM
import           Data.Aeson (toJSON)
import           Data.Proxy
import           Data.List (sortBy)
import           Data.Function (on)
import           Data.Swagger (Swagger)
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import qualified Data.Time.Format as Time

import           Control.Monad
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.Reader (MonadReader, asks, ReaderT, runReaderT)
import           Control.Monad.Catch (MonadThrow, MonadCatch, throwM, catchAll)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.IO.Class (MonadIO)

import           System.Directory (makeAbsolute)

import           Servant
import           Servant.Client
import           Servant.Swagger (toSwagger)
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Logger ()
import           Carma.NominatimMediator.CacheGC
import           Carma.NominatimMediator.CacheSync
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Utils.MonadStatisticsWriter
import           Carma.NominatimMediator.Utils.MonadRequestExecution
import           Carma.NominatimMediator.StatisticsWriter
import           Carma.NominatimMediator.RequestExecutor
import           Carma.Utils.Operators
import           Carma.Monad.LoggerBus.MonadLogger
import           Carma.Monad


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

                  :<|> -- GET /debug/statistics
                       "statistics" :> Get '[JSON] [StatisticsDay]
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

  !(statisticsFile :: Maybe FilePath) <-
    Conf.lookup cfg "cache.synchronizer.statistics-file" >>=
      \case Nothing -> pure Nothing
            Just x  -> Just <$!> makeAbsolute x

  !(noCacheForRevSearch :: Bool) <-
    Conf.lookupDefault True cfg "cache.disable-cache-for-reverse-search"

  !(syncInterval   :: Float) <- Conf.require cfg "cache.synchronizer.interval"
  !(gcInterval     :: Float) <- Conf.require cfg "cache.gc.interval"
  !(cachedLifetime :: Float) <- Conf.require cfg "cache.gc.lifetime"

  !(statisticsLifetime :: Integer) <-
    Conf.require cfg "cache.gc.statistics-lifetime"

  !(nominatimUA :: UserAgent) <-
    UserAgent <$> Conf.require cfg "nominatim.client-user-agent"

  !(nominatimUrl :: String) <-
    Conf.lookupDefault "https://nominatim.openstreetmap.org" cfg "nominatim.url"

  !(nominatimReqGap :: Float) <-
    Conf.require cfg "nominatim.gap-between-requests"

  !(nominatimBaseUrl :: BaseUrl) <- parseBaseUrl nominatimUrl
  !(manager          :: Manager) <- newManager tlsManagerSettings

  resCache            <- newIORefWithCounter mempty
  loggerBus'          <- newEmptyMVar
  requestExecutorBus' <- newEmptyMVar
  statisticsData'     <- newIORefWithCounter mempty
  statisticsBus'      <- newEmptyMVar

  let appCtx
        = AppContext
        { responsesCache              = resCache
        , clientUserAgent             = nominatimUA
        , clientEnv                   = ClientEnv manager nominatimBaseUrl
        , cacheForRevSearchIsDisabled = noCacheForRevSearch
        , loggerBus                   = loggerBus'
        , requestExecutorBus          = requestExecutorBus'
        , statisticsData              = statisticsData'
        , statisticsBus               = statisticsBus'
        }

      -- WARNING! Way to transform monad here is deprecated in newer Servant
      --          version. Read about "hoistServer" from "servant-server" when
      --          you will be migrating from lts-9.21 to newer one.
      app
        = serve (Proxy :: Proxy AppRoutesWithSwagger)
        $ enter withReader appServer
        where
          withReader' :: ReaderT AppContext Handler a -> Handler a
          withReader' r = runReaderT r appCtx

          withReader :: ReaderT AppContext Handler :~> Handler
          withReader = NT withReader'

      warpSettings
        = Warp.defaultSettings
        & Warp.setPort port
        & Warp.setHost (fromString host)

  flip runReaderT appCtx $ do

    -- Running logger thread
    _ <- fork $ runStdoutLoggingT $ writeLoggerBusEventsToMonadLogger

    -- Trying to fill responses cache or/and statistics with initial snapshot
    case (cacheFile, statisticsFile) of
         (Nothing, Nothing) -> pure ()

         (Just cacheFile', Just statisticsFile') ->
           fillCacheWithSnapshot $
             ResponsesCacheAndStatisticsFiles cacheFile' statisticsFile'

         (Just cacheFile', Nothing) ->
           fillCacheWithSnapshot $ OnlyResponsesCacheFile cacheFile'

         (Nothing, Just statisticsFile') ->
           fillCacheWithSnapshot $ OnlyStatisticsFile statisticsFile'

    -- Running cache garbage collector thread
    -- which cleans outdated cached responses.
    _ <- fork $ cacheGCInit gcInterval cachedLifetime statisticsLifetime

    -- Syncing with file is optional, if you don't wanna this feature
    -- just remove "cache-file" or/and "statistics-file" from config.
    -- Running cache synchronizer thread which stores cache snapshot or/and
    -- collected statistics to a file to be able to load it after restart.
    case (cacheFile, statisticsFile) of
         (Just cacheFile', Just statisticsFile') ->
           void $ fork $ cacheSyncInit syncInterval
                $ ResponsesCacheAndStatisticsFiles cacheFile' statisticsFile'

         (Just cacheFile', Nothing) ->
           void $ fork $ cacheSyncInit syncInterval
                $ OnlyResponsesCacheFile cacheFile'

         (Nothing, Just statisticsFile') ->
           void $ fork $ cacheSyncInit syncInterval
                $ OnlyStatisticsFile statisticsFile'

         (Nothing, Nothing) ->
           logInfo [qms| Neither responses cache nor statistics snapshot file
                         is set, synchronizer feature is disabled. |]

    -- Running handler of statistics increments
    _ <- fork statisticsWriterInit

    -- Running requests queue handler
    _ <- fork $ requestExecutorInit nominatimReqGap

    logInfo $
      let
        syncDisabled :: Text -> Text
        syncDisabled x = [qm| not set, {x} synchronizer is disabled |]
      in
        [qmb| Subsystems is initialized.
              GC config:
              \  Checks interval: {floatShow gcInterval} hour(s)
              \  Cached response lifetime: {floatShow cachedLifetime} hour(s)
              Synchronizer:
              \  Responses cache file: \
                   { case cacheFile of
                          Nothing -> syncDisabled "cache"
                          Just x  -> fromString $ show x }
              \  Statistics file: \
                   { case statisticsFile of
                          Nothing -> syncDisabled "statistics"
                          Just x  -> fromString $ show x }
              Nominatim config:
              \  URL: "{nominatimUrl}"
              \  Client User-Agent: "{fromUserAgent nominatimUA}"
              \  Gap between requests: {floatShow nominatimReqGap} second(s) |]

    logInfo [qm| Listening on http://{host}:{port}... |]

  Warp.runSettings warpSettings app


appServer
  :: ( MonadReader AppContext m
     , MonadCatch m
     , MonadBaseControl IO m
     , MonadBase IO m
     , MonadIO m
     )
  => ServerT AppRoutesWithSwagger m
appServer =
  ( search
    :<|> revSearch
    :<|> ( debugCachedQueries
           :<|> debugCachedResponses
           :<|> debugStatistics
         )
  )
  :<|> debugSwaggerAPI


-- Server routes handlers

search
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadCatch m
     , MonadClock m -- For statistics
     , MonadStatisticsWriter m -- To notify about failure cases
     , MonadRequestExecution m
     )
  => Lang -> SearchQuery -> m [SearchByQueryResponse]
search lang query =
  -- Writing statistics about any failure case
  flip catchAll (\e -> writeFailureToStatistics reqType >> throwM e) $ do

  clientUserAgent' <- asks clientUserAgent

  let req = SearchByQueryResponse' <$>
            searchByQuery (Just clientUserAgent')
                          (Just NominatimJSONFormat)
                          (Just lang)
                          (Just query)

  logInfo [qm| Searching by query with params: {reqParams}... |]
  result <- executeRequest reqParams req

  case result of
       (statResolve, SearchByQueryResponse' x) -> do
         utcTime <- getCurrentTime
         x <$ writeStatistics utcTime reqType statResolve

       (_, x) -> throwUnexpectedResponse x

  where reqParams = SearchQueryReq lang query
        reqType   = requestType reqParams

revSearch
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadCatch m
     , MonadClock m -- For statistics
     , MonadStatisticsWriter m -- To notify about failure cases
     , MonadRequestExecution m
     )
  => Lang -> Coords -> m SearchByCoordsResponse
revSearch lang coords@(Coords lon' lat') =
  -- Writing statistics about any failure case
  flip catchAll (\e -> writeFailureToStatistics reqType >> throwM e) $ do

  clientUserAgent' <- asks clientUserAgent

  let req = SearchByCoordsResponse' <$>
            reverseSearchByCoords (Just clientUserAgent')
                                  (Just NominatimJSONFormat)
                                  (Just lang)
                                  (Just $ NominatimLon lon')
                                  (Just $ NominatimLat lat')

  logInfo [qm| Searching by coordinates with params: {reqParams}... |]
  result <- executeRequest reqParams req

  case result of
       (statResolve, SearchByCoordsResponse' x) -> do
         utcTime <- getCurrentTime
         x <$ writeStatistics utcTime reqType statResolve

       (_, x) -> throwUnexpectedResponse x

  where reqParams = RevSearchQueryReq lang coords
        reqType   = requestType reqParams

debugCachedQueries
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadIORefWithCounter m
     )
  => m [DebugCachedQuery]
debugCachedQueries = do
  logInfo "Debugging cached queries..."

  (asks responsesCache >>= readIORefWithCounter)
    <&> -- Ordering by adding to cache time
        M.assocs ? sortBy (compare `on` snd ? fst) ? foldl reducer []

  where reducer acc (k, (t, _)) =
          DebugCachedQuery k [qm| {debugFormatTime t} UTC |] : acc

debugCachedResponses
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadIORefWithCounter m
     )
  => m [DebugCachedResponse]
debugCachedResponses = do
  logInfo "Debugging cached responses..."

  (asks responsesCache >>= readIORefWithCounter)
    <&> -- Ordering by adding to cache time
        M.assocs ? sortBy (compare `on` snd ? fst) ? foldl reducer []

  where
    reducer acc (k, (t, response'))
      = DebugCachedResponse
      { request_params = k
      , time           = [qm| {debugFormatTime t} UTC |]
      , response_type  = requestType response'
      , response       = rjson
      } : acc
      where rjson = case response' of
                         SearchByQueryResponse'  x -> toJSON x
                         SearchByCoordsResponse' x -> toJSON x

debugStatistics
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadIORefWithCounter m
     )
  => m [StatisticsDay]
debugStatistics = do
  logInfo "Reading collected statistics..."
  (asks statisticsData >>= readIORefWithCounter) <&> M.foldlWithKey reducer []

  where
    reducer acc k v = day k v : acc
    reqTypes = Nothing : fmap Just [minBound, maxBound] :: [Maybe RequestType]

    day k v
      = StatisticsDay
      { julian_day      = JulianDay k
      , iso_day         = ISODay k
      , by_request_type =
          reqTypes <&> \x -> StatisticsByRequestType
            { request_type = x
            , statistics   = HM.toList v <&> byReqType x & mconcat
            }
      }

    byReqType
      :: Maybe RequestType
      -> ((RequestType, StatisticResolve), Integer)
      -> RequestsStatistics
    byReqType reqTypeFilter ((reqType, resolve), c) =
      case reqTypeFilter of
           Nothing               -> x
           Just y | y == reqType -> x
                  | otherwise    -> mempty

      where x = mempty { total_requests = c }
                <> succeeded resolve c
                <> failed resolve c

    failed :: StatisticResolve -> Integer -> RequestsStatistics
    failed RequestIsFailed          c = mempty { total_failed = c }
    failed (RequestIsSucceeded _)   _ = mempty
    failed ResponseIsTakenFromCache _ = mempty

    succeeded :: StatisticResolve -> Integer -> RequestsStatistics
    succeeded RequestIsFailed _ = mempty
    succeeded (RequestIsSucceeded True) c
      = mempty
      { total_succeeded               = c
      , succeeded_real_requests       = c
      , succeeded_real_added_to_cache = c
      }
    succeeded (RequestIsSucceeded False) c
      = mempty
      { total_succeeded                   = c
      , succeeded_real_requests           = c
      , succeeded_real_not_added_to_cache = c
      }
    succeeded ResponseIsTakenFromCache c
      = mempty
      { total_succeeded            = c
      , succeeded_taken_from_cache = c
      }

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
  :: (MonadThrow m, MonadReader AppContext m, MonadLoggerBus m)
  => Response -> m a
throwUnexpectedResponse x = do
  logError [qm| Unexpected response result: {x}! |]
  throwM $ UnexpectedResponseResultException x


writeFailureToStatistics
  :: (MonadReader AppContext m, MonadClock m, MonadStatisticsWriter m)
  => RequestType -> m ()
writeFailureToStatistics reqType = do
  utcTime <- getCurrentTime
  writeStatistics utcTime reqType RequestIsFailed


debugFormatTime :: Time.FormatTime t => t -> String
debugFormatTime = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S"
