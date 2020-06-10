{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeApplications #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.Server.ReceiveRequestForServiceRequest
     ( receiveRequestForServiceRequest
     ) where

import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Data.Aeson
import           Data.Time.Clock (addUTCTime)

import           Control.Monad.Reader (MonadReader, ReaderT)
import           Control.Monad.Error.Class (MonadError, catchError)
import           Control.Monad.Catch (MonadCatch)
import           Control.Exception (displayException)

import           Database.Persist ((==.), (>=.), selectFirst, insert)
import           Database.Persist.Sql (SqlBackend, fromSqlKey)
import           Database.Persist.Types

import           Servant

import           Carma.Monad.Clock
import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus.Class (MonadLoggerBus)
import           Carma.Model.Case.Persistent
import           Carma.Model.LegacyTypes
import           Carma.Model.CaseSource.Persistent
import           Carma.Model.CaseStatus.Persistent
import           Carma.Model.Usermeta.Persistent
import           Carma.Model.Program.Persistent
import           Carma.Model.SubProgram.Persistent
import           Carma.Model.Action.Persistent
import           Carma.Model.ActionType.Persistent
import           Carma.Model.Role.Persistent
import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent
import           Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent
import           Carma.EraGlonass.Helpers
import           Carma.EraGlonass.Server.Helpers
import           Carma.EraGlonass.Server.ReceiveRequestForServiceRequest.Helpers
import           Carma.EraGlonass.Types.NonePlug
import           Carma.EraGlonass.Types.Helpers.NonEmptyText
import           Carma.EraGlonass.Types.AppContext (AppContext)
import           Carma.EraGlonass.Types.EGMayFailToParse
import           Carma.EraGlonass.Types.EGRequestForServiceRequest
import           Carma.EraGlonass.Types.EGVin
import           Carma.EraGlonass.Types.EGLatLon
import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( EGIntegrationPoint (RequestForService)
                   )


receiveRequestForServiceRequest
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadError ServerError m
   , MonadCatch m
   , MonadPersistentSql m
   , MonadThread m
   , MonadClock m
   , MonadSTM m
   )
  => EGMayFailToParse EGRequestForServiceRequest
  -> m NonePlug

receiveRequestForServiceRequest (FailedToParse errMsg reqBody) = do
  srcLogError [qmb|
    Failed to parse request body, error message: {errMsg}
    Saving data of this failure to the database in background \
    and responding with HTTP 400 Bad Request error...
  |]

  saveFailureIncidentInBackground reqBody [qm| Error message: {errMsg} |]

  throwError err400 { errBody = [qms|
    Failed to parse request body, error message: {errMsg}
  |] }

receiveRequestForServiceRequest
  (SuccessfullyParsed reqBody@EGRequestForServiceRequest {..}) = NonePlug <$ do
    let caseModel = typeName (Proxy :: Proxy Case) :: Text

    let -- | We need to catch only @ServerError@ because @SomeException@ is
        --   already wrapped by @runSqlProtected@ inside.
        errHandler :: ServerError -> m ()
        errHandler e = do
          saveFailureIncidentInBackground (toJSON reqBody) [qms|
            Acceptance of request for service is failed with exception:
            {displayException e}
          |]

          throwError e -- Throw it back again.

    let handleTransaction :: (UTCTime -> ReaderT SqlBackend m ()) -> m ()
        handleTransaction m = flip catchError errHandler $ do
          time <- getCurrentTime
          runSqlProtected logSrc [qm| Transaction is failed! |] $ m time

    let -- | This code does nothing but keep you informed when new status code
        --   is added to the set of possible status code values, so you should
        --   probably handle it here in some way.
        --
        -- Currently we have only "Sent" from spec and "WorkInProgress" from
        -- real world (see "EGRequestForServiceStatusCode" for details) and it
        -- doesn't matter which one we get.
        _ = case statusCode of
                 Sent           -> ()
                 WorkInProgress -> ()

    handleTransaction $ \time ->
      case vehicle of
           Nothing -> do
             srcLogDebug [qns|
               VIN of a vehicle isn't provided in request body,
               just creating new "Case"...
             |]

             createCase reqBody time

           Just EGRequestForServiceRequestVehicle {..} -> do
             let time24HoursAgo' = time24HoursAgo time
             let shownVin = egVinToString vin :: Text

             srcLogDebug [qms|
               Trying to find already existing Era Glonass "{caseModel}" with
               same car VIN ("{shownVin}") and creation time not older than 24
               hours ({time24HoursAgo'}) to update that "{caseModel}" instead of
               creating new one...
             |]

             lastCaseWithSameVinInLast24Hours <-
               selectFirst
                 [ CaseIsCreatedByEraGlonass ==. True
                 , CaseCar_vin  ==. Just (egVinToString vin)
                 , CaseCallDate >=. Just time24HoursAgo'
                 ]
                 [ Desc CaseCallDate
                 ]

             case lastCaseWithSameVinInLast24Hours of
                  Nothing -> do
                    srcLogDebug [qms|
                      "{caseModel}" with such VIN ("{shownVin}") and not older
                      than 24 hours ({time24HoursAgo'}) not found, creating new
                      "{caseModel}"...
                    |]

                    createCase reqBody time

                  Just Entity { entityKey } -> do
                    srcLogDebug [qms|
                      Found a "{caseModel}" id#{fromSqlKey entityKey} with such
                      VIN ("{shownVin}") and not older than 24 hours
                      ({time24HoursAgo'}), updating it instead of creating new
                      one...
                    |]

                    updateCase reqBody entityKey time


createCase
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadPersistentSql m
   , MonadError ServerError m
   , MonadCatch m
   )
  => EGRequestForServiceRequest
  -> UTCTime
  -> ReaderT SqlBackend m ()

createCase reqBody@EGRequestForServiceRequest {..} time = do
  let caseModel = typeName (Proxy :: Proxy Case) :: Text
  srcLogDebug [qm| Creating new "{caseModel}" for {requestId}... |]

  let programModel    = typeName (Proxy :: Proxy Program)    :: Text
  let subProgramModel = typeName (Proxy :: Proxy SubProgram) :: Text

  srcLogDebug [qms|
    Obtaining any Era Glonass participant "{subProgramModel}" and its
    "{programModel}", because at least "{programModel}" is required field...
  |]

  (anyEGProgram :: ProgramId, anyEGSubProgram :: SubProgramId) <-
    selectFirst [ SubProgramEraGlonassParticipant ==. True
                , SubProgramActive                ==. True
                ] [] >>= \case

      Just subProgram -> pure
        ( subProgramParent $ entityVal subProgram
        , entityKey subProgram
        )

      Nothing -> do
        let logMsg = [qms|
              Not found any active "{subProgramModel}" for "{caseModel}"
              which is Era Glonass participant!
            |]

        srcLogError [qm| {logMsg} |]
        throwError err500 { errBody = logMsg }

  srcLogDebug [qms|
    Era Glonass participant "{subProgramModel}" id#{fromSqlKey anyEGSubProgram}
    and its "{programModel}" id#{fromSqlKey anyEGProgram} are successfully
    obtained. Creating new "{caseModel}"...
  |]

  caseId <-
    insert Case
      { caseCallDate = Just time
      , caseVwcreatedate = Nothing
      , caseCallTaker = admin
      , caseCustomerComment = fccComment <&> fromNonEmptyText
      , caseComment = Nothing

      , caseDiagnosis1 = Nothing
      , caseDiagnosis2 = Nothing
      , caseDiagnosis3 = Nothing
      , caseDiagnosis4 = Nothing

      , caseContact_name = fullName <&> fromNonEmptyText
      , caseContact_phone1 = Phone . fromNonEmptyText <$> phoneNumber
      , caseContact_phone2 = Phone . fromNonEmptyText <$> ivsPhoneNumber
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
      , caseSubprogram = Just anyEGSubProgram

      , caseContractIdentifier = vehicle <&> vin ? egVinToString
      , caseContract = Nothing

      , caseCar_vin = vehicle <&> vin ? egVinToString
      , caseCar_make = Nothing
      , caseCar_model = Nothing
      , caseCar_generation = Nothing
      , caseCar_plateNum = (vehicle >>= plateNumber) <&> fromNonEmptyText
      , caseCar_makeYear = Nothing
      , caseCar_color = Nothing
      , caseCar_buyDate = Nothing
      , caseCar_firstSaleDate = Nothing
      , caseCar_mileage = Nothing
      , caseCar_transmission = Nothing
      , caseCar_engine = Nothing
      , caseCar_liters = Nothing
      , caseCar_class = Nothing

      , caseVinChecked = Nothing
      , caseCity = Nothing
      , caseCaseAddress_city = Nothing
      , caseCaseAddress_address = Nothing
      , caseCaseAddress_comment = (location & description) <&> fromNonEmptyText
      , caseCaseAddress_notRussia = Nothing
      , caseCaseAddress_coords = let
          lon, lat, toAngularMillisecondsCoeff :: Double
          lon = fromIntegral $ fromEGLongitude $ longitude location
          lat = fromIntegral $ fromEGLatitude  $ latitude  location
          toAngularMillisecondsCoeff = 3600 * 1000
          toGradus = (/ toAngularMillisecondsCoeff)
          in Just $ PickerField $ Just [qm| {toGradus lon},{toGradus lat} |]
      , caseCaseAddress_map = Nothing
      , caseTemperature = Nothing
      , caseRepair = Nothing
      , caseAccord = Nothing
      , caseDealerCause = Nothing
      , caseCaseStatus = Carma.Model.CaseStatus.Persistent.back
      , casePsaExportNeeded = Nothing
      , casePsaExported = Nothing
      , caseClaim = Nothing

      , caseFiles = Nothing
      , caseSource = eraGlonass
      , caseAcStart = Nothing
      , caseIsCreatedByEraGlonass = True
      }

  actionId <- createNewAction caseId time
  let actionModel = typeName (Proxy :: Proxy Action) :: Text

  srcLogDebug [qms|
    "{caseModel}" id#{fromSqlKey caseId} is successfully created.
    Also "{actionModel}" id#{fromSqlKey actionId} had been created.
  |]

  let requestModel =
        typeName (Proxy :: Proxy CaseEraGlonassCreateRequest) :: Text

  srcLogDebug [qm| Creating "{requestModel}"... |]

  caseEGCreateRequestId <-
    insert CaseEraGlonassCreateRequest
      { caseEraGlonassCreateRequestCtime          = time
      , caseEraGlonassCreateRequestAssociatedCase = caseId
      , caseEraGlonassCreateRequestRequestId      = requestId
      , caseEraGlonassCreateRequestRequestBody    = toJSON reqBody
      }

  srcLogDebug [qms|
    "{requestModel}" id#{fromSqlKey caseEGCreateRequestId} for "{caseModel}"
    id#{fromSqlKey caseId} is successfully created.
    Creating new "{caseModel}" for {requestId} is done.
  |]


updateCase
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadPersistentSql m
   , MonadError ServerError m
   )
  => EGRequestForServiceRequest
  -> CaseId
  -> UTCTime
  -> ReaderT SqlBackend m ()

updateCase reqBody@EGRequestForServiceRequest {..} caseId time = do
  let caseModel = typeName (Proxy :: Proxy Case) :: Text

  let requestModel =
        typeName (Proxy :: Proxy CaseEraGlonassCreateRequest) :: Text

  srcLogDebug [qms|
    Updating existing "{caseModel}" id#{fromSqlKey caseId} for {requestId}.
    Just creating new "{requestModel}",
    currently it won't affect existing "{caseModel}" anyhow...
  |]

  caseEGCreateRequestId <-
    insert CaseEraGlonassCreateRequest
      { caseEraGlonassCreateRequestCtime          = time
      , caseEraGlonassCreateRequestAssociatedCase = caseId
      , caseEraGlonassCreateRequestRequestId      = requestId
      , caseEraGlonassCreateRequestRequestBody    = toJSON reqBody
      }

  actionId <- createNewAction caseId time
  let actionModel = typeName (Proxy :: Proxy Action) :: Text

  srcLogDebug [qms|
    Successfully updated existing "{caseModel}" id#{fromSqlKey caseId} for
    {requestId}. "{requestModel}" id#{fromSqlKey caseEGCreateRequestId} is
    successfully created.
    Also "{actionModel}" id#{fromSqlKey actionId} had been created.
  |]


createNewAction
  :: MonadPersistentSql m
  => CaseId
  -> UTCTime
  -> ReaderT SqlBackend m ActionId

createNewAction caseId time
  = insert Action
  { actionCallId      = Nothing
  , actionCaseId      = Just caseId
  , actionServiceId   = Nothing
  , actionAType       = tellMeMore
  , actionDuetime     = time
  , actionComment     = Nothing
  , actionRedirectTo  = Nothing
  -- , actionDeferBy     = Nothing
  , actionResult      = Nothing
  , actionCtime       = time
  , actionAssignTime  = Nothing
  , actionOpenTime    = Nothing
  , actionCloseTime   = Nothing
  , actionAssignedTo  = Nothing
  , actionTargetGroup = bo_info
  , actionParent      = Nothing
  }


-- | Takes @UTCTime@ and returns @UTCTime@ 24 hours ago from that moment.
time24HoursAgo :: UTCTime -> UTCTime
time24HoursAgo = addUTCTime $ fromInteger $ (-3600) * 24


saveFailureIncidentInBackground
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadCatch m
   , MonadClock m
   , MonadThread m
   , MonadPersistentSql m
   , MonadSTM m
   )
  => Value
  -> Text
  -> m ()

saveFailureIncidentInBackground requestBody =
  reportToHouston
    (FailureWithBody @'FailureRequestBodyType requestBody)
    RequestForService
