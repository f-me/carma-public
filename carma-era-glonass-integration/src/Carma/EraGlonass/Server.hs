{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, LambdaCase #-}

-- Incoming server implementation to provide an API for Era Glonass side
-- and also some debug stuff for internal usage.
module Carma.EraGlonass.Server
     ( serverApplicaton
     ) where

import           Data.Proxy
import           Data.Swagger (Swagger)
import           Text.InterpolatedString.QM
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Aeson (toJSON)

import           Control.Monad
import           Control.Monad.Reader (MonadReader, asks, runReaderT, ReaderT)
import           Control.Monad.Error.Class (MonadError, throwError, catchError)
import           Control.Monad.Random.Class (MonadRandom (..))

import           Servant
import           Servant.Swagger (toSwagger)

import           Database.Persist ((==.))
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Types

import           Carma.Utils.Operators
import           Carma.Monad.Clock
import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus
import           Carma.Monad.PersistentSql
import           Carma.Model.Program.Persistent
import           Carma.Model.SubProgram.Persistent
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Routes
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent


type FaliuresAPI
    =  -- GET /debug/failures/count.json
       "count.json" :> Get '[JSON] Word

  :<|> -- GET /debug/failures/list.json?limit=10
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
     , MonadClock m
     , MonadRandom m
     , MonadThread m
     )
  => ServerT ServerAPI m
server = egCRM01 :<|> (swagger :<|> getFailuresCount :<|> getFailuresList)


type EgCrm01Monad m =
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadError ServantErr m
   , MonadPersistentSql m
   , MonadClock m
   , MonadRandom m
   , MonadThread m
   )

-- | EG.CRM.01 integration point handler.
--
-- WARNING! For failure cases it returns 200 HTTP status with
--          @EGCreateCallCardResponse@ which have @acceptCode@ that is not @OK@.
egCRM01
  :: EgCrm01Monad m
  => EGCreateCallCardRequest
  -> m EGCreateCallCardResponse

egCRM01 (EGCreateCallCardRequestIncorrect msg badReqBody) = do
  logError [qmb| {EgCrm01}: Failed to parse request body, error message: {msg}
                 Saving data of this failure to the database in separated \
                 thread and returning response immediately... |]

  logDebug [qms| {EgCrm01}: Saving failure data to the database
                            (but returning proper response notwithstanding
                             if this failure data saving is succeeded or
                             failed by running it in another thread)... |]

  _ <- fork $ do -- Saving failure data in another thread.
    time <- getCurrentTime

    failureId <-
      runSqlProtected
        [qm| {EgCrm01}: Failed to save failure data to the database! |]
        $ insert CaseEraGlonassFailure
        { caseEraGlonassFailureCtime = time
        , caseEraGlonassFailureIntegrationPoint = EgCrm01
        , caseEraGlonassFailureRequestBody = Just badReqBody
        , caseEraGlonassFailureResponseId = Nothing
        , caseEraGlonassFailureComment = Just [qm| Error message: {msg} |]
        }

    logError [qms| {EgCrm01}:
                   Failure data is successfully saved to the database.
                   Failure id: {failureId} |]

  logDebug [qms| {EgCrm01}: Producing random response id
                            for failure response... |]

  randomResponseId <- getRandomResponseId

  logError [qms| {EgCrm01}: Response id for failure response is:
                            "{randomResponseId}" |]

  pure EGCreateCallCardResponseFailure
     { responseId = randomResponseId
     , acceptCode = IncorrectFormat
     , statusDescription = Just "400 Bad Request"
     }

egCRM01 reqBody@EGCreateCallCardRequest {..} = handleFailure $ do

  logDebug [qms| {logPfx} Attempt to find any "Program" which have "SubProgram"
                 which is Era Glonass participant (since "Program" is required
                 field of "Case" model so we couldn't leave it empty) |]

  !(anyEGProgram :: ProgramId) <-
    runSqlProtected
      [qms| {logPfx} Failed to request "SubProgram"
                     which is Era Glonass participant! |]
      (selectFirst [SubProgramEraGlonassParticipant ==. True] [])
      >>= \case Just x  -> pure $ subProgramParent $ entityVal x
                Nothing -> do
                  let logMsg = [qns| Not found any "Program" for "Case"
                                     which is Era Glonass participant
                                     (have some "SubProgram" which is
                                      Era Glonass participant). |]

                  logError [qm| {logPfx} {logMsg} |]
                  throwError err500 { errBody = logMsg }

  logDebug [qms| {logPfx} Era Glonass participant "Program" is successfully
                 obtained: {anyEGProgram} |]

  -- logDebug [qm| {logPfx} Creating CaRMa "Case"... |]
  -- TODO create "Case"

  randomResponseId <- getRandomResponseId
  logDebug [qm| {logPfx} Response id: "{randomResponseId}" |]

  pure EGCreateCallCardResponse
     { responseId        = randomResponseId
     , cardidProvider    = "case id"
     , acceptId          = fromEGCallCardId cardIdCC
     , requestId         = requestId
     , acceptCode        = OK
     , statusDescription = Nothing
     }

  where
    logPfx :: Text
    logPfx = [qms| Incoming Creating Call Card request
                   (Call Card id: "{fromEGCallCardId cardIdCC}",
                    Request id: "{fromRequestId requestId}"): |]

    handleFailure
      :: EgCrm01Monad m
      => m EGCreateCallCardResponse
      -> m EGCreateCallCardResponse
    handleFailure m =
      m `catchError` \exception -> do
        logError [qms| {logPfx} Request handler is failed
                                with exception: {exception} |]

        logDebug [qms| {logPfx} Producing random response id
                                for failure response... |]

        randomResponseId <- getRandomResponseId

        logError [qms| {logPfx} Response id for failure response is:
                                "{randomResponseId}" |]

        logDebug [qms| {logPfx} Saving failure data to the database
                                (but returning proper response notwithstanding
                                 if this failure data saving is succeeded or
                                 failed by running it in another thread)... |]

        _ <- fork $ do -- Saving failure data in another thread.
          time <- getCurrentTime

          failureId <-
            runSqlProtected
              [qm| {logPfx} Failed to save failure data to the database! |]
              $ insert CaseEraGlonassFailure
              { caseEraGlonassFailureCtime = time
              , caseEraGlonassFailureIntegrationPoint = EgCrm01
              , caseEraGlonassFailureRequestBody = Just $ toJSON reqBody
              , caseEraGlonassFailureResponseId = Just randomResponseId
              , caseEraGlonassFailureComment = Just
                  [qm| Request handler is failed, exception: {exception} |]
              }

          logDebug [qms| {logPfx}
                         Failure data is successfully saved to the database.
                         Failure id: {failureId} |]

        pure EGCreateCallCardResponseFailure
           { responseId = randomResponseId
           , acceptCode = InternalError
           , statusDescription = Just "Request handling is failed"
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
    fromIntegral <$>
      runSqlProtected
        [qm| Failed to request EG failures total count! |]
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
      [qm| Failed to request EG failures list! |]
      $ selectList [] [ Desc CaseEraGlonassFailureId
                      , LimitTo $ fromIntegral n
                      ]

  logDebug [qm| EG failures list is obtained, total elements: {length result} |]
  pure result


runSqlProtected
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadPersistentSql m
     , MonadError ServantErr m
     )
  => Text -- ^ Fail message
  -> ReaderT SqlBackend m a
  -> m a

runSqlProtected errMsg =
  runSqlInTime >=> \case
    Just x  -> pure x
    Nothing -> do
      let logMsg = [qm| Database request is failed: {errMsg} |]
      logError [qm| {logMsg} |]
      throwError err500 { errBody = logMsg }


runSqlInTime
  :: (MonadReader AppContext m, MonadPersistentSql m)
  => ReaderT SqlBackend m a
  -> m (Maybe a)

runSqlInTime m = asks dbRequestTimeout >>= flip runSqlTimeout m


getRandomResponseId :: MonadRandom m => m Text
getRandomResponseId =
  getRandomRs (0, pred $ length randomChars)
    <&> fmap (randomChars !!) ? take responseIdLength ? fromString
  where
    randomChars = ['a'..'z'] <> ['0'..'9'] :: String
    responseIdLength = 50 -- ^ Chars count
