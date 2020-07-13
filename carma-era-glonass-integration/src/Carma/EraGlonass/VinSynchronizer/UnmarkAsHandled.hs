{-# LANGUAGE FlexibleContexts, ViewPatterns, NamedFieldPuns, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.VinSynchronizer.UnmarkAsHandled
     ( unmarkAsHandled
     ) where

import           Data.Proxy
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Data.List.NonEmpty (NonEmpty)

import           Control.Monad (forM_)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch (MonadThrow (throwM))

import           Database.Persist ((=.), (==.))
import           Database.Persist.Sql (SqlBackend, updateWhere, update)

import           Carma.Monad
import           Carma.Monad.LoggerBus.Class
import           Carma.Model.Contract.Persistent
import           Carma.Utils
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent
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
       (ContractId, EGVin)
       (EraGlonassSynchronizedContractId, EGVin)
  -> (Word, Word, Word)
  -- ^ Total VINs count, no longer handled VINs count, ephemeral VINs count.
  -> ReaderT SqlBackend m ()

unmarkAsHandled lists (totalCount, contractsCount, ephemeralsCount) = do
  srcLogDebug [qmb|
    Received some VINs to "unmark" them as handled by us.
    Counts of VINs which are supposed to be "unmarked":
    \  Total: {totalCount}
    \  {contractsLogInfix}: {contractsCount}
    \  {ephemeralsLogInfix}: {ephemeralsCount}
  |]

  unbindRequest <-
    lift (asks vinSynchronizerContractId) <&> \contractId' ->
      let
        allVins :: NonEmpty EGVin
        allVins =
          case lists of
               FirstNonEmptyList  xs    -> fmap snd xs
               SecondNonEmptyList    ys -> fmap snd ys
               BothNonEmptyLists  xs ys -> fmap snd xs <> fmap snd ys
      in
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
             let alreadyUnboundVins :: [Text]
                 alreadyUnboundVins = errors <&> \case
                   EGBindVehiclesResponseError
                     { vin, errorCode = VinNotFound } -> egVinToString vin

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
               "Unmarking" {totalCount} VINs as handled by us is failed due to
               failure case in the response from EG service, result code is
               {resultCode}, textual failure description{descriptionText}
             |]

         srcLogError [qm| {errorMsg} |]
         throwM $ FailureScenario errorMsg

  currentTime <- lift getCurrentTime

  srcLogDebug [qms|
    {totalCount} VINs are successfully "unmarked" as handled by us by request to
    Era Glonass service. Now "unmarking" {totalCount} those VINs on our side...
  |]

  let unmarkUpdate =
        [ EraGlonassSynchronizedContractIsHandledByCarma     =. False
        , EraGlonassSynchronizedContractLastStatusChangeTime =. Just currentTime
        ]

  getFirstNonEmptyList lists `possibly` \contracts -> do
    srcLogDebug [qms|
      "Unmarking" {contractsCount} {contractsLogInfix} on our side...
    |]

    forM_ contracts $ \(id', vin) ->
      updateWhere [ EraGlonassSynchronizedContractContract ==. id'
                  , EraGlonassSynchronizedContractVin ==. egVinToString vin
                  ] unmarkUpdate

  getSecondNonEmptyList lists `possibly` \ephemerals -> do
    srcLogDebug [qms|
      "Unmarking" {ephemeralsCount} {ephemeralsLogInfix} on our side...
    |]

    forM_ ephemerals $ \(id', _) -> update id' unmarkUpdate

  srcLogDebug [qn| Successfully finished "unmarking" VINs. |]


contractsLogInfix :: Text
contractsLogInfix = go where
  go = [qm| outdated/deactivated "{modelName}"s |]
  modelName = typeName (Proxy :: Proxy Contract) :: Text

ephemeralsLogInfix :: Text
ephemeralsLogInfix = go where
  go = [qm| "ephemeral VINs" ("{modelName}"s) |]
  modelName = typeName (Proxy :: Proxy EraGlonassSynchronizedContract) :: Text
