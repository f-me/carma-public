{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Incoming server implementation to provide an API for Era Glonass side
-- and also some debug stuff for internal usage.
module Carma.EraGlonass.Server
     ( serverApplicaton
     ) where

import           Data.Proxy
import           Data.Swagger (Swagger)
import           Text.InterpolatedString.QM

import           Control.Monad.Reader (MonadReader, runReaderT, ReaderT)
import           Control.Monad.Error.Class (MonadError, throwError)

import           Servant
import           Servant.Swagger (toSwagger)

import           Database.Persist.Types

import           Carma.Utils.Operators
import           Carma.Monad.LoggerBus
import           Carma.Monad.PersistentSql
import           Carma.EraGlonass.Logger () -- instance
import           Carma.EraGlonass.Persistent () -- instance
import           Carma.EraGlonass.Routes
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent


type FaliuresAPI
    =  -- GET /debug/failures/count.json
       "count.json" :> Get '[JSON] Word

  :<|> -- GET /debug/failures/list.json
       "list.json" :>
       QueryParam "limit" Word :>
       Get '[JSON] [Entity CaseEraGlonassFailure]


type ServerAPI
    =  IncomingAPI
  :<|> "debug" :> (    -- GET /debug/swagger.json
                       "swagger.json" :> Get '[JSON] Swagger

                  :<|> "failures" :> FaliuresAPI
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


server
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadError ServantErr m
     , MonadPersistentSql m
     )
  => ServerT ServerAPI m
server = egCRM01 :<|> (swagger :<|> getFailuresCount :<|> getFailuresList)


egCRM01
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadError ServantErr m
     , MonadPersistentSql m
     )
  => EGCreateCallCardRequest
  -> m EGCreateCallCardResponse

egCRM01 (EGCreateCallCardRequestIncorrect msg badReqBody) = do
  logError [qmb| {iPoint}: Failed to parse request body, error message: {msg}
                 Saving data of this failure to the database... |]

  failureId <- runSql
    $ insert CaseEraGlonassFailure
    { caseEraGlonassFailureIntegrationPoint = iPoint
    , caseEraGlonassFailureRequestBody      = Just badReqBody
    , caseEraGlonassFailureComment          = Just [qm| Error message: {msg} |]
    }

  logError [qms| {iPoint}: Failure data successfully saved to the database.
                 Failure id: {failureId} |]

  throwError err400
    { errBody = [qm| Error while parsing {iPoint} request JSON data. |] }

  where iPoint = EgCrm01

egCRM01 EGCreateCallCardRequest {..} = do
  logError "TODO EG.CRM.01 SUCCESS"

  -- TODO create "Case"
  -- TODO generate some "responseId"

  pure EGCreateCallCardResponse
    { responseId        = "some random stuff"
    , cardidProvider    = "case id"
    , acceptId          = cardIdCC & \(EGCallCardId x) -> x
    , requestId         = requestId
    , acceptCode        = OK
    , statusDescription = Nothing
    }


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
    fromIntegral <$> runSql (count ([] :: [Filter CaseEraGlonassFailure]))

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

  result <- runSql $
    selectList [] [ Desc CaseEraGlonassFailureId
                  , LimitTo $ fromIntegral n
                  ]

  logDebug [qm| EG failures list is obtained, total elements: {length result} |]
  pure result
