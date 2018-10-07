{-# LANGUAGE DuplicateRecordFields, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | EG.CRM.01 handler module.
module Carma.EraGlonass.Server.EgCrm01
     ( egCRM01
     ) where

import           Data.Monoid ((<>))
import           Data.Text (Text, intercalate, unpack)
import           Data.Text.Encoding (decodeUtf8)
import           Text.InterpolatedString.QM
import           Data.Time.Clock (addUTCTime)
import           Data.Aeson (toJSON)

import           Control.Monad.Reader (MonadReader, ReaderT)
import           Control.Monad.Error.Class (MonadError, throwError, catchError)
import           Control.Monad.Random.Class (MonadRandom)

import           Servant

import           Database.Persist ((==.), (>=.))
import           Database.Persist.Sql (SqlBackend, fromSqlKey)
import           Database.Persist.Types

import           Carma.Monad.STM
import           Carma.Monad.MVar
import           Carma.Monad.Clock
import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus
import           Carma.Monad.PersistentSql
import           Carma.Model.Case.Persistent
import           Carma.Model.CaseSource.Persistent
import           Carma.Model.CaseStatus.Persistent
import           Carma.Model.City.Persistent
import           Carma.Model.Usermeta.Persistent
import           Carma.Model.Program.Persistent
import           Carma.Model.SubProgram.Persistent
import           Carma.Model.LegacyTypes
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Types.PersistentTextKey
import           Carma.EraGlonass.Types.EGCreateCallCardAcceptCode
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent
import           Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent
import           Carma.EraGlonass.Server.Helpers


-- | EG.CRM.01 monads constraint.
type EgCrm01Monad m =
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


-- | EG.CRM.01 integration point handler.
--
-- WARNING! For failure cases it returns 200 HTTP status with
--          @EGCreateCallCardResponse@ which have @acceptCode@ that is not @OK@.
egCRM01
  :: EgCrm01Monad m
  => EGCreateCallCardRequest
  -> m EGCreateCallCardResponse

egCRM01 (EGCreateCallCardRequestIncorrect msg badReqBody) = do
  logError [qmb|
    {EgCrm01}: Failed to parse request body, error message: {msg}
    Saving data of this failure to the database in separated \
    thread and returning response...
  |]

  logDebug [qms|
    {EgCrm01}: Saving failure data to the database
    (but returning proper response notwithstanding
     if this failure data saving is succeeded or
     failed by running it in background)...
  |]

  -- Saving failure data in background
  inBackground $ do
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

    logError [qmb|
      {EgCrm01}: Failure data is successfully saved to the database:
      \  Failure id: {fromSqlKey failureId}.
    |]

  randomResponseId <- getRandomResponseId

  logError
    [qm| {EgCrm01}: Response id for failure response is: "{randomResponseId}" |]

  pure EGCreateCallCardResponseFailure
     { responseId = randomResponseId
     , acceptCode = IncorrectFormat
     , statusDescription = Just "400 Bad Request"
     }

egCRM01 reqBody@EGCreateCallCardRequest {..} = handleFailure $ do
  time        <- getCurrentTime
  responseId' <- ResponseId <$> getRandomResponseId

  runSqlProtected [qm| {logPfx} Transaction is failed! |] $ do

    let time24HoursAgo' = time24HoursAgo time

    prefixedLog logDebug [qms|
      Trying to find already existing Era Glonass "Case" with same car VIN
      ("{fromEGVin $ vin (vehicle :: EGCreateCallCardRequestVehicle)}")
      and creation time not older than 24 hours ({time24HoursAgo'})
      to use that "Case" instead of creating new one...
    |]

    lastCaseWithSameVinInLast24Hours <-
      selectFirst
        [ CaseIsCreatedByEraGlonass ==. True
        , CaseCar_vin ==. let
            x = vin (vehicle :: EGCreateCallCardRequestVehicle)
            in Just $ decodeUtf8 $ fromEGVin x
        , CaseCallDate >=. Just time24HoursAgo'
        ]
        [ Desc CaseCallDate
        ]

    case lastCaseWithSameVinInLast24Hours of
         Nothing -> do
           prefixedLog logDebug [qms|
             Creation time: "{time}", response id: "{responseId'}".
             Finding any "Program" which have "SubProgram" which is Era Glonass
             participant (since "Program" is required field of "Case" model
             so we couldn't leave it empty) then creating "Case" and
             "CaseEraGlonassCreateRequest" in single transaction...
           |]

           createCase reqBody prefixedLog time responseId'

         Just Entity { entityKey } -> do
           prefixedLog logDebug [qms|
             Creation time (of new "CaseEraGlonassCreateRequest"
             for already existing "Case" {fromSqlKey entityKey}): "{time}",
             response id: "{responseId'}".
             Adding new "CaseEraGlonassCreateRequest"
             for already existing "Case": {fromSqlKey entityKey}...
           |]

           updateCase reqBody entityKey prefixedLog time responseId'

  where
    logPfx :: Text
    logPfx = [qms|
      Incoming Creating Call Card request
      (Call Card id: "{fromEGCallCardId cardIdCC}",
       Request id: "{fromRequestId requestId}"):
    |]

    prefixedLog
      :: MonadLoggerBus mlogger
      => (Text -> mlogger ())
      -> (Text -> mlogger ())

    prefixedLog logFn msg = logFn $ logPfx <> " " <> msg

    handleFailure
      :: EgCrm01Monad m
      => m EGCreateCallCardResponse
      -> m EGCreateCallCardResponse
    handleFailure m =
      m `catchError` \exception -> do
        prefixedLog logError
          [qm| Request handler is failed with exception: {exception} |]

        randomResponseId <- getRandomResponseId

        prefixedLog logError
          [qm| Response id for failure response is: "{randomResponseId}" |]

        prefixedLog logDebug [qms|
          Saving failure data to the database
          (but returning proper response notwithstanding
           if this failure data saving is succeeded or
           failed by running it in background)...
        |]

        -- Saving failure data in background
        inBackground $ do
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

          prefixedLog logDebug [qmb|
            Failure data is successfully saved to the database:
            \  Failure id: {fromSqlKey failureId}.
          |]

        pure EGCreateCallCardResponseFailure
           { responseId = randomResponseId
           , acceptCode = InternalError
           , statusDescription = Just
               [qm| Request handling is failed with exception: {exception} |]
           }


-- | Handler of regular case when we're creating new
-- 'Carma.Model.Case.Persistent.Case' and
-- 'Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent.CaseEraGlonassCreateRequest'.
createCase
  :: EgCrm01Monad m
  => EGCreateCallCardRequest
  -> (  forall mlogger . MonadLoggerBus mlogger
     => (Text -> mlogger ())
     -> (Text -> mlogger ())
     )
  -> UTCTime
  -> ResponseId
  -> ReaderT SqlBackend m EGCreateCallCardResponse

createCase reqBody@EGCreateCallCardRequestIncorrect {} prefixedLog _ _ = do
  let logMsg = [qm| Unexpected request body constructor: {reqBody} |]
  prefixedLog logError [qm| {logMsg} |]
  throwError err500 { errBody = logMsg }

createCase reqBody@EGCreateCallCardRequest {..}
           prefixedLog
           time
           responseId' = do

  prefixedLog logDebug [qn|
    Obtaining any Era Glonass participant "SubProgram" and its "Program"...
  |]

  (anyEGProgram :: ProgramId, anyEGSubProgram :: SubProgramId) <-
    selectFirst [SubProgramEraGlonassParticipant ==. True] [] >>= \case

      Just subProgram -> pure
        ( subProgramParent $ entityVal subProgram
        , entityKey subProgram
        )

      Nothing -> do
        let logMsg = [qns|
              Not found any "SubProgram" for "Case"
              which is Era Glonass participant!
            |]

        prefixedLog logError [qm| {logMsg} |]
        throwError err500 { errBody = logMsg }

  prefixedLog logDebug [qmb|
    Era Glonass participant "SubProgram" \
    and its "Program" are successfully obtained:
    \  "SubProgram" id: {fromSqlKey anyEGSubProgram};
    \  "Program" id: {fromSqlKey anyEGProgram}.
  |]

  maybeCityId <-
    case gis of
         [] -> pure Nothing

         (EGCreateCallCardRequestGis { settlementName } : _) -> do
           prefixedLog logDebug [qmb|
             Trying to find a "City" with label equals to "{settlementName}"
             to fill "Case" fields
             "city" and "caseAddress_city" with that "CityId"...
           |]

           foundCity <- selectFirst [ CityLabel ==. settlementName ] []

           case foundCity of
                Nothing ->
                  Nothing <$ prefixedLog logDebug [qmb|
                    "City" with label "{settlementName}" not found.
                  |]
                Just Entity {..} ->
                  Just entityKey <$ prefixedLog logDebug [qmb|
                    "City" with label "{settlementName}" \
                    is successfully obtained:
                    \  "City" id: {fromSqlKey entityKey}
                  |]

  prefixedLog logDebug [qn| Creating "Case"... |]

  caseId <-
    insert Case
      { caseCallDate = Just time
      , caseVwcreatedate = Nothing
      , caseCallTaker = admin
      , caseCustomerComment = Nothing
      , caseComment = Nothing

      , caseDiagnosis1 = Nothing
      , caseDiagnosis2 = Nothing
      , caseDiagnosis3 = Nothing
      , caseDiagnosis4 = Nothing

      , caseContact_name = Just $ fromEGCallerFullName callerFullName
      , caseContact_phone1 =
          Just $ Phone $ fromEGPhoneNumber callerPhoneNumber
      , caseContact_phone2 = Just $ Phone $ fromEGPhoneNumber atPhoneNumber
      , caseContact_phone3 = Nothing
      , caseContact_phone4 = Nothing
      , caseContact_email = Nothing
      , caseContact_contactOwner = Nothing
      , caseContact_ownerName = Nothing
      , caseContact_ownerPhone1 = Nothing
      , caseContact_ownerPhone2 = Nothing
      , caseContact_ownerPhone3 = Nothing
      , caseContact_ownerPhone4 = Nothing
      , caseContact_ownerEmail = Nothing

      , caseProgram = anyEGProgram
      , caseSubprogram = Nothing

      , caseContractIdentifier =
          Just $ decodeUtf8 $ fromEGVin $
            vin (vehicle :: EGCreateCallCardRequestVehicle)
      , caseContract = Nothing

      , caseCar_vin =
          Just $ decodeUtf8 $ fromEGVin $
            vin (vehicle :: EGCreateCallCardRequestVehicle)
      , caseCar_make = Nothing
      , caseCar_model = Nothing
      , caseCar_generation = Nothing
      , caseCar_plateNum = Just $ registrationNumber vehicle
      , caseCar_makeYear = Nothing
      , caseCar_color = Just $ color vehicle
      , caseCar_buyDate = Nothing
      , caseCar_firstSaleDate = Nothing
      , caseCar_mileage = Nothing
      , caseCar_transmission = Nothing
      , caseCar_engine = egPropulsionToEngineId <$> propulsion vehicle
      , caseCar_liters = Nothing
      , caseCar_class = Nothing

      , caseVinChecked = Nothing
      , caseCity = maybeCityId
      , caseCaseAddress_city = maybeCityId
      , caseCaseAddress_address =
          case gis of
               [] -> Nothing
               (EGCreateCallCardRequestGis {..} : _) -> let
                 partsList =
                   filter (/= mempty) $
                     ( if regionName == settlementName
                          then [regionName]
                          else [regionName, settlementName]
                     ) <> [streetName, building]
                 in Just $ PickerField $ Just $ intercalate ", " partsList
      , caseCaseAddress_comment = Just locationDescription
      , caseCaseAddress_notRussia = Nothing
      , caseCaseAddress_coords = let
          lon, lat, toAngularMillisecondsCoeff :: Double
          lon = fromIntegral $ fromEGLongitude lastTrustedLongitude
          lat = fromIntegral $ fromEGLatitude lastTrustedLatitude
          toAngularMillisecondsCoeff = 3600 * 1000
          toGradus = (/ toAngularMillisecondsCoeff)
          in Just $ PickerField $ Just [qm| {toGradus lon},{toGradus lat} |]
      , caseCaseAddress_map = Nothing
      , caseTemperature = Nothing
      , caseRepair = Nothing
      , caseAccord = Nothing
      , caseDealerCause = Nothing
      , caseCaseStatus = front
      , casePsaExportNeeded = Nothing
      , casePsaExported = Nothing
      , caseClaim = Nothing

      , caseFiles = Nothing
      , caseSource = eraGlonass
      , caseAcStart = Nothing
      , caseIsCreatedByEraGlonass = True
      }

  prefixedLog logDebug [qmb|
    "Case" is successfully created:
    \  "Case" id: {fromSqlKey caseId}.
  |]

  prefixedLog logDebug [qn| Creating "CaseEraGlonassCreateRequest"... |]

  caseEGCreateRequestId <-
    insert CaseEraGlonassCreateRequest
      { caseEraGlonassCreateRequestCtime          = time
      , caseEraGlonassCreateRequestAssociatedCase = caseId
      , caseEraGlonassCreateRequestRequestId      = requestId
      , caseEraGlonassCreateRequestCallCardId     = cardIdCC
      , caseEraGlonassCreateRequestResponseId     = fromResponseId responseId'
      , caseEraGlonassCreateRequestRequestBody    = reqBody
      }

  prefixedLog logDebug [qmb|
    "CaseEraGlonassCreateRequest" is successfully created:
    \  "CaseEraGlonassCreateRequest" id: {fromSqlKey caseId}.
  |]

  prefixedLog logDebug [qmb|
    "Case" and "CaseEraGlonassCreateRequest" are successfully created:
    \  Found Era Glonass participant "SubProgram" id: \
         {fromSqlKey anyEGSubProgram};
    \  Found "Program" id: {fromSqlKey anyEGProgram};
    \  "Case" id: {fromSqlKey caseId};
    \  "CaseEraGlonassCreateRequest" id: {fromSqlKey caseEGCreateRequestId}.
  |]

  prefixedLog logDebug [qn| Responding about success... |]

  pure EGCreateCallCardResponse
     { responseId        = fromResponseId responseId'
     , cardidProvider    = PersistentTextKey caseId
     , acceptId          = cardIdCC
     , requestId         = requestId
     , acceptCode        = OK
     , statusDescription = Nothing
     }


-- | Handler of a sitation when we already received Call Card with same VIN in
-- last 24 hours.
--
-- In such case:
--
--   * Creating new
--     'Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent.CaseEraGlonassCreateRequest';
--   * Updating 'Carma.Model.Case.Persistent.Case' history
--     (with shown info from Call Card);
--   * Creating \"urgent matter" 'Carma.Model.Action.Action';
--   * If there's some 'Carma.Model.Action.Action' without assigned responsible
--     or there's no open 'Carma.Model.Action.Action' then trigger new
--     'Carma.Model.Action.Action'
--     according to Back Office 'Carma.Model.Action.Action' logic.
--
updateCase
  :: EgCrm01Monad m
  => EGCreateCallCardRequest
  -> CaseId
  -> (  forall mlogger . MonadLoggerBus mlogger
     => (Text -> mlogger ())
     -> (Text -> mlogger ())
     )
  -> UTCTime
  -> ResponseId
  -> ReaderT SqlBackend m EGCreateCallCardResponse

updateCase reqBody@EGCreateCallCardRequestIncorrect {} _ prefixedLog _ _ = do
  let logMsg = [qm| Unexpected request body constructor: {reqBody} |]
  prefixedLog logError [qm| {logMsg} |]
  throwError err500 { errBody = logMsg }

updateCase reqBody@EGCreateCallCardRequest {..}
           caseId
           prefixedLog
           time
           responseId' = do

  prefixedLog logDebug [qn| Creating "CaseEraGlonassCreateRequest"... |]

  caseEGCreateRequestId <-
    insert CaseEraGlonassCreateRequest
      { caseEraGlonassCreateRequestCtime          = time
      , caseEraGlonassCreateRequestAssociatedCase = caseId
      , caseEraGlonassCreateRequestRequestId      = requestId
      , caseEraGlonassCreateRequestCallCardId     = cardIdCC
      , caseEraGlonassCreateRequestResponseId     = fromResponseId responseId'
      , caseEraGlonassCreateRequestRequestBody    = reqBody
      }

  prefixedLog logDebug [qmb|
    "CaseEraGlonassCreateRequest" is successfully created:
    \  "CaseEraGlonassCreateRequest" id: {fromSqlKey caseEGCreateRequestId}.
  |]

  pure EGCreateCallCardResponse
     { responseId        = fromResponseId responseId'
     , cardidProvider    = PersistentTextKey caseId
     , acceptId          = cardIdCC
     , requestId         = requestId
     , acceptCode        = OK
     , statusDescription = Just [qms|
         Already existing CaRMa "Case" is updated.
         "Case" id: {fromSqlKey caseId},
         new "CaseEraGlonassCreateRequest" id:
         {fromSqlKey caseEGCreateRequestId}.
       |]
     }


time24HoursAgo :: UTCTime -> UTCTime
time24HoursAgo = addUTCTime $ fromInteger $ (-3600) * 24


newtype ResponseId = ResponseId { fromResponseId :: Text }
instance Show ResponseId where show = unpack . fromResponseId
