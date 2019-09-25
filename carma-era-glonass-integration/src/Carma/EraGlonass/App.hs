{-# LANGUAGE OverloadedStrings, QuasiQuotes, DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, DataKinds #-}
{-# LANGUAGE BangPatterns, LambdaCase, DeriveAnyClass #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Carma.EraGlonass.App
     ( AppConfig (..)
     , app
     ) where

import           Prelude hiding (fail)

import           Data.Function ((&))
import           Data.Typeable
import           Data.Time.LocalTime (getCurrentTimeZone)
import qualified Data.Configurator as Conf
import           Data.String (fromString)
import           Text.InterpolatedString.QM

import           Control.Applicative
import           Control.Monad hiding (fail)
import           Control.Monad.Reader hiding (fail)
import           Control.Monad.Logger
                   ( LoggingT
                   , runStdoutLoggingT
                   , runStderrLoggingT
                   )
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Random.Class (MonadRandom)
import           Control.Monad.Catch
import           Control.Monad.Fail (MonadFail (fail))
import           Control.Concurrent.MVar (tryReadMVar)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TSem
import           Control.Exception
                   ( Exception (displayException)
                   , SomeException (..)
                   )

import           Foreign.C.Types (CInt (..))

import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitFailure)
import           System.Posix.Signals
                   ( installHandler
                   , Handler (Catch)
                   , sigINT
                   , sigTERM
                   )

import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Database.Persist.Postgresql (PostgresConf (PostgresConf))

import           Servant.Client (ClientEnv (ClientEnv), BaseUrl, parseBaseUrl)

import           Carma.Utils.Operators
import           Carma.Utils.MonadLogger.Syslog (runSyslogLoggingT)
import           Carma.Monad.LoggerBus.Types (LogMessage)
import           Carma.Monad.LoggerBus.MonadLogger
import           Carma.Monad.LoggerBus
import           Carma.Monad.Thread
import           Carma.Monad.Delay
import           Carma.Monad.STM
import           Carma.Monad.Concurrently
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Server (serverApplicaton)
import           Carma.EraGlonass.VinSynchronizer (runVinSynchronizer)
import           Carma.EraGlonass.StatusSynchronizer (runStatusSynchronizer)
import           Carma.EraGlonass.Types.AppContext
import           Carma.EraGlonass.Types.EGContractId (EGContractId)
import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( EGIntegrationPoint (BindVehicles, ChangeProcessingStatus)
                   )

foreign import ccall "exit" exit :: CInt -> IO ()


-- | Application config data to provide to particular implementation.
data AppConfig
   = AppConfig
   { pgConf :: PostgresConf
   , dbRequestTimeout :: Float
   }


-- | Application starter which abstract from specific database.
--
-- It takes a monad that wraps server runner and provides specific database
-- connection to the server. Server runner constructs @AppContext@ with provided
-- database connection and runs a server.
--
-- About monad constraint: top-level abstract @IO@ monad,
-- all sub-systems has explicit monads set abstracted from any @IO@
-- (they mustn't have any @IO@, @MonadIO@ or @MonadBaseControl IO@ in their
-- dependencies).
app
  :: forall m
   .
   ( MonadIO m
   , MonadBaseControl IO m
   , MonadRandom m
   , MonadThrow m
   , MonadCatch m
   , MonadMask m
   , MonadFail m
   )
  => AppMode
  -> ( AppConfig
       -> (DBConnection -> ReaderT (TQueue LogMessage) m ())
       -> ReaderT (TQueue LogMessage) m ()
     )
  -- ^ Database connection creator that wraps monad that depends on database
  -> m ()
app appMode' withDbConnection = do
  timeZone <- liftIO getCurrentTimeZone

  cfg <- liftIO $ Conf.load [Conf.Required "app.cfg"]

  !(port :: Warp.Port) <- liftIO $ Conf.require cfg "port"
  !(host :: String)    <- liftIO $ Conf.lookupDefault "127.0.0.1" cfg "host"

  !(egBaseUrl :: BaseUrl) <-
    parseBaseUrl =<< liftIO (Conf.require cfg "eg-base-url")

  -- It required to construct @ClientEnv@ alongwith @BaseUrl@.
  !(manager :: Manager) <- liftIO $ newManager tlsManagerSettings

  !pgConf' <- liftIO $ PostgresConf
    <$> Conf.require cfg "db.postgresql.connection-string"
    <*> Conf.require cfg "db.postgresql.pool-size"

  -- In seconds
  !(dbRequestTimeout' :: Float) <-
    liftIO $ Conf.require cfg "db.postgresql.request-timeout"

  -- In seconds
  !(vinSynchronizerTimeout' :: Float) <-
    liftIO $ Conf.require cfg "vin-synchronizer.timeout"

  -- In minutes
  !(vinSynchronizerRetryInterval' :: Float) <-
    liftIO $ Conf.require cfg "vin-synchronizer.retry-interval"

  !(vinSynchronizerBatchSize' :: Word) <-
    liftIO $ Conf.require cfg "vin-synchronizer.batch-size"

  !(carmaVinSynchronizerContractId :: EGContractId 'BindVehicles) <-
    liftIO $ Conf.require cfg "vin-synchronizer.carma-contract-id"

  -- In minutes
  !(statusSynchronizerInterval' :: Float) <-
    liftIO $ Conf.require cfg "status-synchronizer.interval"

  -- In seconds
  !(statusSynchronizerTimeout' :: Float) <-
    liftIO $ Conf.require cfg "status-synchronizer.timeout"

  !( carmaStatusSynchronizerContractId ::
     Maybe (EGContractId 'ChangeProcessingStatus) ) <-
       liftIO $ Conf.lookup cfg "status-synchronizer.carma-contract-id"

  !(loggerSink :: LoggingT m () -> m ()) <- liftIO $
    Conf.require cfg "logger.sink" >>= \case
      "stdout" -> pure runStdoutLoggingT
      "stderr" -> pure runStderrLoggingT
      "syslog" -> runSyslogLoggingT <$> Conf.require cfg "logger.syslog-ident"

      (x :: String) ->
        let errMsg = [qm| Unexpected logger sink value: "{x}" |]
         in error errMsg <$ hPutStrLn stderr errMsg `finally` exit 1

  loggerBus' <- atomically newTQueue

  -- Running logger thread
  (_, loggerThreadWaitBus) <-
    forkWithWaitBus $ do
      let logReader = writeLoggerBusEventsToMonadLogger `runReaderT` loggerBus'
      loggerSink logReader `catch` \(SomeException e) ->
        liftIO $ hPutStrLn stderr [qms|
          Logger reader thread is failed with an unexpected exception:
          {displayException e}
        |] `finally` exit 1

  let appConfig
        = AppConfig
        { pgConf           = pgConf'
        , dbRequestTimeout = dbRequestTimeout'
        }

  ( backgroundTasksCounter',
    vinSynchronizerTriggerBus',
    statusSynchronizerTriggerBus',
    workersThreadFailureSem )
      <- atomically $
           (,,,) <$> newTVar 0 <*> newEmptyTMVar <*> newEmptyTMVar <*> newTSem 0

  (workersThreadId, workersThreadSem) <-
    flip runReaderT loggerBus'
      $ forkWithSem
      $ handle (\e@(SomeException _) -> logError [qm|
          Workers thread is unexpectedly failed with exception: {e}
        |] `finally` atomically (signalTSem workersThreadFailureSem))
      $ withDbConnection appConfig
      $ \dbConnection' -> do

          let appContext
                = AppContext
                { appMode = appMode'
                , loggerBus = loggerBus'

                , dbConnection = dbConnection'
                , dbRequestTimeout = round $ dbRequestTimeout' * (10 ** 6)

                , backgroundTasksCounter = backgroundTasksCounter'

                , egClientEnv = ClientEnv manager egBaseUrl
                , vinSynchronizerContractId = carmaVinSynchronizerContractId

                , statusSynchronizerContractId =
                    carmaStatusSynchronizerContractId

                , vinSynchronizerTimeout =
                    round $ vinSynchronizerTimeout' * (10 ** 6)
                , vinSynchronizerRetryInterval =
                    round $ vinSynchronizerRetryInterval' * 60 * (10 ** 6)
                , vinSynchronizerBatchSize = vinSynchronizerBatchSize'
                , vinSynchronizerTriggerBus = vinSynchronizerTriggerBus'

                , statusSynchronizerInterval =
                    round $ statusSynchronizerInterval' * 60 * (10 ** 6)
                , statusSynchronizerTimeout =
                    round $ statusSynchronizerTimeout' * (10 ** 6)
                , statusSynchronizerTriggerBus = statusSynchronizerTriggerBus'
                }

          -- Semaphore for case when some worker is unexpectedly failed
          failureSem <- atomically $ newTSem 0

          -- Running server thread
          (serverThreadId, serverThreadSem) <-
            forkWithSem $ handle (\e@(SomeException _) -> logError [qms|
              Incoming HTTP-server thread is unexpectedly failed
              with exception: {e}
            |] `finally` atomically (signalTSem failureSem)) $ do

              when (appMode' == TestingAppMode) $
                logWarn
                  "Starting testing server with in-memory SQLite database..."

              logInfo [qm| Running incoming server on http://{host}:{port}... |]
              liftIO $ runIncomingServer appContext port $ fromString host

          -- Running VIN synchronizer thread
          vinSynchronizer <-
            if appMode' == TestingAppMode
               then do logWarn [qns| Not running VIN synchronizer
                                     because it is testing mode. |]
                       pure Nothing

               else fmap Just $ forkWithSem
                              $ handle (\e@(SomeException _) -> logError [qms|
                                  VIN synchronizer worker thread is
                                  unexpectedly failed with exception: {e}
                                |] `finally` atomically (signalTSem failureSem))
                              $ do

                      logInfo "Running VIN synchronizer worker thread..."
                      runVinSynchronizer timeZone `runReaderT` appContext

          statusSynchronizer <-
            if appMode' == TestingAppMode
               then do logWarn [qns| Not running Status Synchronizer
                                     because it is testing mode. |]
                       pure Nothing

               else fmap Just $ forkWithSem
                              $ handle (\e@(SomeException _) -> logError [qms|
                                  Status Synchronizer worker thread is
                                  unexpectedly failed with exception: {e}
                                |] `finally` atomically (signalTSem failureSem))
                              $ do

                      logInfo "Running Status Synchronizer worker thread..."
                      runStatusSynchronizer `runReaderT` appContext

          let waitForWorkers = runConcurrently x where
                waitFor = atomically . waitTSem
                x = failure <|> workers

                failure
                  = Concurrently
                  $ waitFor failureSem
                    >> fail [qm|
                         Some of threads of workers is unexpectedly failed!
                       |]

                workers = void $ (,,)
                  <$> Concurrently
                      ( -- Wait for server thread
                        waitFor serverThreadSem
                      )
                  <*> Concurrently
                      ( -- Wait for VIN synchronizer thread
                        maybe (pure ()) (snd ? waitFor) vinSynchronizer
                      )
                  <*> Concurrently
                      ( -- Wait for Status Synchronizer thread
                        maybe (pure ()) (snd ? waitFor) statusSynchronizer
                      )

          waitForWorkers `catch` \e@KillWorkersException -> do

            logDebug
              [qm| Caught {e} in workers thread, killing workers threads... |]

            logDebug [qn| Killing server thread... |]
            killThread serverThreadId

            case vinSynchronizer of
                 Nothing -> pure ()
                 Just (threadId, _) -> do
                   logDebug [qn| Killing VIN synchronizer thread... |]
                   killThread threadId

            case statusSynchronizer of
                 Nothing -> pure ()
                 Just (threadId, _) -> do
                   logDebug [qn| Killing Status Synchronizer thread... |]
                   killThread threadId

  -- Trapping termination of the application
  liftIO $ forM_ [sigINT, sigTERM] $ \sig -> let
    terminateHook = throwTo workersThreadId KillWorkersException
    in installHandler sig (Catch terminateHook) Nothing

  -- Wait for workers thread
  runConcurrently $
    let waitFor = atomically . waitTSem
    in  Concurrently (waitFor workersThreadFailureSem >> liftIO exitFailure)
    <|> Concurrently (waitFor workersThreadSem)

  let waitForLogger =
        atomically (tryPeekTQueue loggerBus') >>= \case
          Nothing -> pure () -- We're done, successfully exiting
          Just _ -> -- Logger still have something to handle
            tryReadMVar loggerThreadWaitBus >>= \case
              Nothing ->
                -- Waiting for 100 milliseconds before next iteration
                -- to avoid high CPU usage.
                delay (100 * 1000) >> waitForLogger
              Just _ ->
                -- Something went wrong
                fail [qns| Logger thread is probably failed
                           before it finished handling all log messages! |]

  -- Making sure that logger handled all the messages from logger bus
  liftIO waitForLogger


runIncomingServer :: AppContext -> Warp.Port -> Warp.HostPreference -> IO ()
runIncomingServer appContext port host = go where
  go
    = Warp.runSettings warpSettings
    $ serverApplicaton appContext

  warpSettings
    = Warp.defaultSettings
    & Warp.setPort port
    & Warp.setHost host


data KillWorkersException
   = KillWorkersException
     deriving (Show, Typeable, Exception)
