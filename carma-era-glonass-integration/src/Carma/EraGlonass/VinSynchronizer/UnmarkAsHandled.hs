{-# LANGUAGE FlexibleContexts, ViewPatterns, NamedFieldPuns, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.VinSynchronizer.UnmarkAsHandled
     ( unmarkAsHandled
     ) where

import           Data.Proxy
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Data.List.NonEmpty (NonEmpty, nub, toList)
import           Data.Either.Combinators (mapLeft)

import           Control.Monad (unless)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch (MonadThrow (throwM))
import           Control.Exception (AssertionFailed (AssertionFailed))

import           Database.Persist ((=.), (<-.))
import           Database.Persist.Class (persistFieldDef)
import           Database.Persist.Sql (SqlBackend, fromSqlKey)

import           Database.Persist.Types
                   ( Entity (..)
                   , FieldDef (fieldHaskell)
                   , HaskellName
                   )

import           Carma.Monad
import           Carma.Model.Contract.Persistent
import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Model.EraGlonassSynchronizedContract.Persistent
import           Carma.EraGlonass.Types.AppContext (AppContext (..))
import           Carma.EraGlonass.Types.EGBindVehiclesRequest
import           Carma.EraGlonass.Types.EGVin
import           Carma.EraGlonass.Types.EGMayFailToParse
import           Carma.EraGlonass.Types.Helpers.NonEmptyText
import           Carma.EraGlonass.VinSynchronizer.Types
import           Carma.EraGlonass.VinSynchronizer.Helpers
import           Carma.EraGlonass.Client (unbindVehicles)


unmarkAsHandled
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadPersistentSql m
   , MonadServantClient m
   , MonadThrow m
   , MonadClock m
   )
  => OneOrTwoNonEmptyLists
       (Entity Contract)
       (Entity EraGlonassSynchronizedContract)
  -> ReaderT SqlBackend m ()

unmarkAsHandled lists = do
  let (contractsLength, ephemeralsLength) = oneOrTwoNonEmptyListsLengths lists

  srcLogDebug [qmb|
    Received some VINs to "unmark" them as handled by us.
    Counts of VINs which are supposed to be "unmarked":
    \  {contractsLogInfix}: {contractsLength}
    \  {ephemeralsLogInfix}: {ephemeralsLength}
  |]

  let -- | Obtaining valid @EGVin@s from regular @Text@ fields.
      extrudeEphemeralVins
        :: forall t. t ~ EraGlonassSynchronizedContract
        => NonEmpty (Entity t)
        -> m (NonEmpty EGVin)

      extrudeEphemeralVins = go where
        go = either (throwM . AssertionFailed) pure . sequenceA . fmap parse

        parse ( Entity id' EraGlonassSynchronizedContract
                             { eraGlonassSynchronizedContractVin = vin } )
          = parseFailureErr id' `mapLeft` stringToProvedEGVin vin

        fieldName :: EntityField t typ -> HaskellName
        fieldName = fieldHaskell . persistFieldDef

        modelName   = typeName (Proxy :: Proxy t) :: String
        idFieldName = fieldName EraGlonassSynchronizedContractId

        parseFailureErr id' (msg :: String) = [qms|
          {modelName} {idFieldName}#{fromSqlKey id'}
          unexpectedly has incorrect VIN. Parsing error message: {msg}
        |]

  (allVins :: NonEmpty EGVin) <- lift $
    case lists of
         (FirstNonEmptyList x) -> extrudeContractVINs x
         (SecondNonEmptyList y) -> extrudeEphemeralVins y
         (BothNonEmptyLists x y) ->
            (<>) <$> extrudeContractVINs x <*> extrudeEphemeralVins y

  let allVinsLen = length allVins

  do
    let sumLen = contractsLength + ephemeralsLength

    unless (fromIntegral allVinsLen /= sumLen) $
      throwM $ AssertionFailed [qms|
        Count of all parsed "EGVin"s ({allVinsLen}) unexpectedly isn't equal to
        sum of {contractsLogInfix} ({contractsLength}) and
        {ephemeralsLogInfix} ({ephemeralsLength}): {sumLen},
        something went wrong.
      |]

  do
    let countOfUniqueVins = length $ nub allVins

    unless (allVinsLen /= countOfUniqueVins) $
      throwM $ AssertionFailed [qms|
        Count of all parsed "EGVin"s ({allVinsLen}) unexpectedly isn't equal to
        count of unique elements of that list ({countOfUniqueVins}), all VINs
        are supposed to be unique, something went wrong.
      |]

  unbindRequest <-
    lift (asks carmaEgServiceCode) <&> \contractId' ->
      EGBindVehiclesRequest
        { contractId = contractId'
        , vins       = allVins
        }

  response <-
    lift (asks egClientEnv)
      >>= lift . runClientM (unbindVehicles unbindRequest)
      >>= either throwM pure
      >>= \case SuccessfullyParsed x -> pure x
                FailedToParse err body ->
                  throwM $ ResponseParseFailure [qms|
                    Failed to parse the response of an attempt to "unmark"
                    some VINs as handled by us with this error message: {err}
                  |] body

  case response of
       -- We don't actually have to case about @errors@ list, currently only
       -- possible kinda \"error" is @VinNotFound@ which means a VIN is already
       -- \"unmarked" as handled by us, os it's okay.
       --
       -- Just matching exact constructor in case this list would be extended in
       -- the future and the compiler will tell us we need to handle new
       -- constructor here.
       --
       -- Also we're logging which VINs were \"unmarked" already.
       EGBindVehiclesResponseUnbindOk { errors }
         | null errors -> pure ()
         | otherwise   -> do
             let alreadyUnboundVins = errors <&> \case
                   EGBindVehiclesResponseError
                     { vin, errorCode = VinNotFound } -> vin

             srcLogWarn [qms|
               These VINs were already "unmarked" as handled by us
               at the time of trying to "unmark" them: {alreadyUnboundVins},
               we're taking this as okay, but maybe it was an error
               of a previous VINs synchronization attempt?
             |]

       EGBindVehiclesResponseFailure { resultCode, description } -> do
         let descriptionText =
               maybe " was not provided"
                     ((": " <>) . fromNonEmptyText)
                     description

         let errorMsg = [qms|
               "Unmarking" {allVinsLen} VINs as handled by us is failed due to
               failure case in the response from EG service, result code is
               {resultCode}, textual failure description{descriptionText}
             |]

         srcLogError [qm| {errorMsg} |]
         throwM $ FailureScenario errorMsg

  currentTime <- lift getCurrentTime

  srcLogDebug [qns|
    VINs are successfully "unmarked" as handled by us by request to Era Glonass
    service. Now "unmarking" them on our side...
  |]

  let unmarkUpdate =
        [ EraGlonassSynchronizedContractIsHandledByCarma     =. False
        , EraGlonassSynchronizedContractLastStatusChangeTime =. Just currentTime
        ]

  flip (maybe $ pure ()) (getSecondNonEmptyList lists) $ \ephemerals -> do
    srcLogDebug [qm| "Unmarking" {ephemeralsLogInfix} on our side... |]
    let ids = toList $ ephemerals <&> \Entity { entityKey } -> entityKey
    updateWhere [EraGlonassSynchronizedContractId <-. ids] unmarkUpdate

  flip (maybe $ pure ()) (getFirstNonEmptyList lists) $ \contracts -> do
    srcLogDebug [qm| "Unmarking" {contractsLogInfix} on our side... |]
    let ids = toList $ contracts <&> \Entity { entityKey } -> entityKey
    updateWhere [EraGlonassSynchronizedContractContract <-. ids] unmarkUpdate

  srcLogDebug [qn| Successfully finished "unmarking" VINs. |]


contractsLogInfix :: Text
contractsLogInfix = go where
  go = [qm| outdated/deactivated "{modelName}"s |]
  modelName = typeName (Proxy :: Proxy Contract) :: Text

ephemeralsLogInfix :: Text
ephemeralsLogInfix = go where
  go = [qm| "ephemeral VINs" ("{modelName}"s) |]
  modelName = typeName (Proxy :: Proxy EraGlonassSynchronizedContract) :: Text
