{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Incoming server implementation to provide an API for Era Glonass side
-- and also some debug stuff for internal usage.
module Carma.EraGlonass.Server
     ( serverApplicaton
     ) where

import           Data.Proxy
import           Data.Swagger (Swagger)
import           Text.InterpolatedString.QM

import           Control.Monad.Reader (MonadReader, asks, runReaderT, ReaderT)
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad.Random.Class (MonadRandom)
import           Control.Concurrent.STM.TVar

import           Servant
import           Servant.Swagger (toSwagger)

import           Database.Persist.Types

import           Carma.Monad.STM
import           Carma.Monad.MVar
import           Carma.Monad.Clock
import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus
import           Carma.Monad.PersistentSql
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Routes
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent
import           Carma.EraGlonass.Server.Helpers
import           Carma.EraGlonass.Server.EgCrm01


type FaliuresAPI
  =    -- GET /debug/failures/count.json
       "count.json" :> Get '[JSON] Word

  :<|> -- GET /debug/failures/list.json?limit=10
       "list.json" :>
       QueryParam "limit" Word :>
       Get '[JSON] [Entity CaseEraGlonassFailure]


type ServerAPI
  =    IncomingAPI

  :<|> "debug" :> (    -- GET /debug/swagger.json
                       "swagger.json" :> Get '[JSON] Swagger

                  :<|> "failures" :> FaliuresAPI

                  :<|> -- GET /debug/background-tasks/count.json
                       "background-tasks" :> "count.json" :> Get '[JSON] Word
                  )


-- | All monads constraints of all handlers.
type ServerMonad m =
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadError ServantErr m
   , MonadPersistentSql m
   , MonadClock m
   , MonadRandom m
   , MonadThread m
   , MonadMVar m
   , MonadSTM m
   )


-- WARNING! Way to transform monad here is deprecated in newer Servant version.
--          Read about "hoistServer" from "servant-server" when you will be
--          migrating from lts-9.21 to newer one.
serverApplicaton :: AppContext -> Application
serverApplicaton appContext =
  serve (Proxy :: Proxy ServerAPI) $ enter withReader server

  where withReader' :: ReaderT AppContext Handler a -> Handler a
        withReader' r = runReaderT r appContext

        withReader :: ReaderT AppContext Handler :~> Handler
        withReader = NT withReader'


server :: ServerMonad m => ServerT ServerAPI m
server
  =    egCRM01
  :<|> (    swagger
       :<|> (getFailuresCount :<|> getFailuresList)
       :<|> getBackgroundTasksCount
       )


swagger :: Applicative m => m Swagger
swagger = pure $ toSwagger (Proxy :: Proxy IncomingAPI)


getFailuresCount
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadError ServantErr m
     , MonadPersistentSql m
     )
  => m Word

getFailuresCount = do
  logDebug [qn| Obtaining EG failures total count... |]

  totalCount <-
    fromIntegral <$>
      runSqlProtected
        [qn| Failed to request EG failures total count! |]
        (count ([] :: [Filter CaseEraGlonassFailure]))

  logDebug [qm| Total EG failures is obtained: {totalCount} |]
  pure totalCount


getFailuresList
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadError ServantErr m
     , MonadPersistentSql m
     )
  => Maybe Word
  -> m [Entity CaseEraGlonassFailure]

getFailuresList Nothing = do
  logError [qn| Attempt to obtain EG failures list without specified limit! |]

  throwError err400
    { errBody = [qns| Getting EG failures list
                      without specified limit isn't allowed! |] }

getFailuresList (Just n) = do
  logDebug [qm| Obtaining EG failures list limited to last {n} elements... |]

  result <-
    runSqlProtected
      [qn| Failed to request EG failures list! |]
      $ selectList [] [ Desc CaseEraGlonassFailureId
                      , LimitTo $ fromIntegral n
                      ]

  logDebug [qm| EG failures list is obtained, total elements: {length result} |]
  pure result


getBackgroundTasksCount
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadSTM m
     )
  => m Word

getBackgroundTasksCount = do
  logDebug [qn| Reading background tasks counter... |]
  result <- asks backgroundTasksCounter >>= atomically . readTVar
  logDebug [qm| Background tasks count: {result} |]
  pure result
