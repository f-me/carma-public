{-# LANGUAGE FlexibleContexts, OverloadedStrings, NamedFieldPuns, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, QuasiQuotes, TemplateHaskell #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.VinSynchronizer.SynchronizeContracts
     ( synchronizeContracts
     ) where

import           Prelude hiding (id)

import           Text.InterpolatedString.QM
import           Data.List.NonEmpty (NonEmpty)

import           Control.Monad (foldM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch (MonadThrow (throwM))

import           Database.Persist.Sql (SqlBackend, selectFirst, insert_, update)
import           Database.Persist.Types (Entity (..))
import           Database.Persist ((=.), (==.))
import           Database.Esqueleto (Single(..), unSingle)

import           Carma.Monad
import           Carma.Monad.LoggerBus.Class
import           Carma.Model.Contract.Persistent
import           Carma.Utils.Operators
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent (MonadPersistentSql)
import           Carma.EraGlonass.Model.EraGlonassSynchronizedContract.Persistent
import           Carma.EraGlonass.Types.AppContext (AppContext (..))
import           Carma.EraGlonass.Types.EGBindVehiclesRequest
import           Carma.EraGlonass.Types.EGMayFailToParse
import           Carma.EraGlonass.Types.EGVin
import           Carma.EraGlonass.Types.Helpers.NonEmptyText
import           Carma.EraGlonass.VinSynchronizer.Types
import           Carma.EraGlonass.VinSynchronizer.Helpers
import           Carma.EraGlonass.Client (bindVehicles)


synchronizeContracts
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadPersistentSql m
   , MonadServantClient m
   , MonadClock m
   , MonadThrow m
   )
  => NonEmpty (Single ContractId, Single EGVin)
  -> Word
  -> ReaderT SqlBackend m ()

synchronizeContracts vinsToMark vinsToMarkCount = do
  srcLogDebug [qm| Marking {vinsToMarkCount} VINs as handled by us... |]

  bindRequest <-
    lift (asks vinSynchronizerContractId) <&> \contractId' ->
      EGBindVehiclesRequest
        { contractId = contractId'
        , vins       = vinsToMark <&> snd ? unSingle
        }

  response <-
    lift (asks egClientEnv)
      >>= lift . runClientM (bindVehicles bindRequest)
      >>= either throwM pure
      >>= \case SuccessfullyParsed x -> pure x
                FailedToParse err body ->
                  throwM $ ResponseParseFailure [qms|
                    Failed to parse the response of an attempt to mark
                    some VINs as handled by us with this error message: {err}
                  |] body

  case response of
       EGBindVehiclesResponseBindOk ->
         srcLogDebug [qms|
           {vinsToMarkCount} VINs were successfully marked as handled by us to
           Era Glonass service. Now we need to mark them as handled by us on our
           side...
         |]

       EGBindVehiclesResponseFailure { resultCode, description } -> do
         let descriptionText =
               maybe " was not provided"
                     ((": " <>) . fromNonEmptyText)
                     description

         let errorMsg = [qms|
               Marking {vinsToMarkCount} VINs as handled by us is failed due
               to failure case in the response from EG service, result code is
               {resultCode}, textual failure description{descriptionText}
             |]

         srcLogError [qm| {errorMsg} |]
         throwM $ FailureScenario errorMsg

  currentTime <- lift getCurrentTime

  let reducer (newCount, updatedCount) (Single id', Single vin) = do
        foundPrevious <-
          selectFirst
            [ EraGlonassSynchronizedContractContract ==. id'
            , EraGlonassSynchronizedContractVin      ==. egVinToString vin
            ] []

        case foundPrevious of
             Nothing ->
               (succ newCount, updatedCount) <$
                 insert_ EraGlonassSynchronizedContract
                   { eraGlonassSynchronizedContractCtime = currentTime
                   , eraGlonassSynchronizedContractContract = id'
                   , eraGlonassSynchronizedContractVin = egVinToString vin
                   , eraGlonassSynchronizedContractIsHandledByCarma = True
                   , eraGlonassSynchronizedContractLastStatusChangeTime =
                       Nothing
                   }

             Just Entity { entityKey } ->
               (newCount, succ updatedCount) <$
                 update entityKey
                   [ EraGlonassSynchronizedContractIsHandledByCarma =. True
                   , EraGlonassSynchronizedContractLastStatusChangeTime =.
                       Just currentTime
                   ]

  foldM reducer (minBound :: Word, minBound :: Word) vinsToMark >>= \case
    (newCount, updatedCount) ->
      srcLogDebug [qms|
        Created {newCount} new synchronization entities in the database for
        new VINs marked as handled by us and updated {updatedCount}
        previously added entities.
      |]
