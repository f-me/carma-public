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

import           Control.Monad.Reader (MonadReader, runReaderT, ReaderT)
import           Control.Monad.Error.Class (MonadError, throwError, catchError)
import           Control.Monad.Random.Class (MonadRandom (..))

import           Servant
import           Servant.Swagger (toSwagger)

import           Database.Persist ((==.))
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

egCRM01
  :: EgCrm01Monad m
  => EGCreateCallCardRequest
  -> m EGCreateCallCardResponse

egCRM01 (EGCreateCallCardRequestIncorrect msg badReqBody) = do
  logError [qmb| {EgCrm01}: Failed to parse request body, error message: {msg}
                 Saving data of this failure to the database... |]

  time <- getCurrentTime

  failureId <- runSql
    $ insert CaseEraGlonassFailure
    { caseEraGlonassFailureCtime            = time
    , caseEraGlonassFailureIntegrationPoint = EgCrm01
    , caseEraGlonassFailureRequestBody      = Just badReqBody
    , caseEraGlonassFailureComment          = Just [qm| Error message: {msg} |]
    , caseEraGlonassFailureResponseId       = Nothing
    }

  logError [qms| {EgCrm01}: Failure data is successfully saved to the database.
                 Failure id: {failureId} |]

  throwError err400
    { errBody = [qm| Error while parsing {EgCrm01} request JSON data. |] }

egCRM01 reqBody@EGCreateCallCardRequest {..} = handleFailure $ do

  logDebug [qms| {logPfx} Attempt to find any "Program" which have "SubProgram"
                 which is Era Glonass participant (since "Program" is required
                 field of "Case" model so we couldn't leave it empty) |]

  !(anyEGProgram :: ProgramId) <-
    runSql (selectFirst [SubProgramEraGlonassParticipant ==. True] [])
      >>= \case Just x  -> pure $ subProgramParent $ entityVal x
                Nothing ->
                  throwError err500
                    { errBody = [qns| Not found any "Program" for "Case"
                                      which is Era Glonass participant
                                      (have some "SubProgram" which is
                                       Era Glonass participant). |] }

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
    getRandomResponseId :: MonadRandom m => m Text
    getRandomResponseId =
      getRandomRs (0, pred $ length randomChars)
        <&> fmap (randomChars !!) ? take 50 ? fromString

      where randomChars = ['a'..'z'] <> ['0'..'9'] :: String

    logPfx :: Text
    logPfx = [qms| Incoming Creating Call Card request
                   (Call Card id: "{fromEGCallCardId cardIdCC}",
                    Request id: "{fromRequestId requestId}"): |]

    handleFailure
      :: EgCrm01Monad m
      => m EGCreateCallCardResponse
      -> m EGCreateCallCardResponse
    handleFailure m =
      m `catchError` \(exception :: ServantErr) -> do
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
        _ <- fork $ do
          time <- getCurrentTime

          failureId <- runSql
            $ insert CaseEraGlonassFailure
            { caseEraGlonassFailureCtime = time
            , caseEraGlonassFailureIntegrationPoint = EgCrm01
            , caseEraGlonassFailureRequestBody = Just $ toJSON reqBody
            , caseEraGlonassFailureComment = Just "Request handler is failed"
            , caseEraGlonassFailureResponseId = Just randomResponseId
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
