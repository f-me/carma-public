{-# LANGUAGE OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields, LambdaCase, TupleSections #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ConstraintKinds #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | \"Synchronize contracts" action module.
module Carma.EraGlonass.VinSynchronizer.SynchronizeContracts
     ( synchronizeContracts
     ) where

import           Data.Monoid
import           Data.Typeable
import           Data.Time.Calendar (Day)
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Random.Class (MonadRandom)
import           Control.Monad.Catch

import           Database.Persist ((==.), (!=.), (<.), (<-.), (/<-.), (||.))
import           Database.Persist.Types (Entity (..))
import           Database.Persist.Sql (SqlBackend, fromSqlKey)

import           Carma.Monad
import           Carma.Utils.Operators
import           Carma.Model.SubProgram.Persistent
import           Carma.Model.Contract.Persistent
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Client
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.Model.EraGlonassSynchronizedContract.Persistent
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types
import           Carma.EraGlonass.VinSynchronizer.Types


type SynchronizeContractsMonad m =
     ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadClock m
     , MonadPersistentSql m
     , MonadThrow m
     , MonadRandom m -- For creating new @RequestId@
     , MonadServantClient m
     )



-- | \"Synchronize contracts" action implementation.
--
-- It will obtain "Contract"s which belong to Era Glonass participant
-- "SubProgram" and not yet synchronized, and \"mark" them on EG side as handled
-- by CaRMa (which means to synchronize them).
synchronizeContracts
  :: SynchronizeContractsMonad m
  => Day -- ^ Current UTC day
  -> [Entity EraGlonassSynchronizedContract] -- ^ List of already handled VINs
  -> [SubProgramId] -- ^ All active EG participant "SubProgram"s
  -> ReaderT SqlBackend m ()

synchronizeContracts nowDay handledContracts egSubPrograms = do

  (contractsToSynchronize :: [Entity Contract]) <- let

    handledVINs =
      handledContracts <&> eraGlonassSynchronizedContractVin . entityVal

    selectFilter =
      [ ContractIsActive ==. True
      , ContractVin !=. Nothing

      -- Not already handled
      , ContractVin /<-. fmap Just handledVINs

      -- EG participant
      , ContractSubprogram <-. fmap Just egSubPrograms
      ]
      <> stillValidFilter

    stillValidFilter
      =   [ ContractValidUntil ==. Nothing ]
      ||. [ ContractValidUntil !=. Nothing
          , ContractValidUntil <.  Just nowDay
          ]

    in selectList selectFilter []

  (syncResponse :: EGAddVinResponse) <- lift $ let

    req requestsList reqId
      = EGAddVinRequest
      { requestId = reqId
      , requests  = requestsList
      }

    vinsList =
      contractsToSynchronize <&>
        \Entity { entityKey = contractKey
                , entityVal = Contract { contractVin }
                } ->

          case contractVin <&> textToProofedEGVin of
               Just x@(Right _) -> x

               Just (Left msg) -> Left [qms|
                 VIN of a "{contractTypeRep}" with id {fromSqlKey contractKey}
                 is incorrect. Error message: {msg}
               |]

               Nothing -> Left [qms|
                 Unexpectedly VIN of a "{contractTypeRep}" with id
                 {fromSqlKey contractKey} is not set ("Nothing").
                 It supposed to be checked earlier in database selection filter.
               |]

    mkReq
      :: SynchronizeContractsMonad m
      => m (EGAddVinRequest, Either ServantError EGAddVinResponse)

    mkReq = do
      requestsList <-
        forM vinsList $ \case
          Right vin ->
            pure EGAddVinRequestRequests { vin = vin }
          Left msg ->
            logError [qm| {CrmEg02} is failed due to: {msg} |] >> fail msg

      req' <- req requestsList <$> newRequestId
      (req',) <$> (runClientM (crmEG02Put req') =<< asks egClientEnv)

    handleReqFailure
      :: SynchronizeContractsMonad m
      => EGAddVinRequest
      -> Either ServantError EGAddVinResponse
      -> m EGAddVinResponse

    handleReqFailure req' = \case
      Right x -> pure x -- Success

      Left e -> do
        logError [qmb|
          {CrmEg02} is failed on PUT request to Era Glonass to synchronize \
          "{contractTypeRep}"s (VINs) which are handled by us.
          Error: {e}
        |]

        throwM $ EGAddVinRequestIsFailed e req'

    in mkReq >>= uncurry handleReqFailure

  -- TODO implement
  syncResponse `seq` fail [qnb|
    TODO implement data correctness checking for response
    TODO implement synchronizing on our side
    TODO implement partial synchronizing looking at EG response
  |]


contractTypeRep :: TypeRep
contractTypeRep = typeRep (Proxy :: Proxy Contract)
