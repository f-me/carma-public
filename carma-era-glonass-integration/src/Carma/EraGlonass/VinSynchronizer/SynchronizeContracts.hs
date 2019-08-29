{-# LANGUAGE FlexibleContexts, OverloadedStrings, NamedFieldPuns, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, GADTs, QuasiQuotes, TemplateHaskell #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

module Carma.EraGlonass.VinSynchronizer.SynchronizeContracts
     ( synchronizeContracts
     ) where

import           Prelude hiding (id)

import           Data.Proxy
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Data.List (genericLength)
import           Data.List.NonEmpty (NonEmpty ((:|)), toList, partition)
import           Data.Foldable (find)
import           Data.Time.Calendar (Day)

import           Control.Monad (forM, forM_)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch (MonadThrow (throwM), MonadCatch)
import           Control.Exception (AssertionFailed (AssertionFailed))

import           Database.Persist.Sql (SqlBackend, fromSqlKey)
import           Database.Persist.Types (Entity (..))
import           Database.Persist
                   ( (<-.), (/<-.), (!=.), (=.), (||.), (<.), (==.)
                   )

import           Carma.Monad
import           Carma.Model.Contract.Persistent
import           Carma.Model.SubProgram.Persistent
import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Helpers (FailureBody (FailureWithoutBody))
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
   , MonadCatch m
   , MonadThread m
   , MonadSTM m
   )
  => Day
  -- ^ Current day, when synchronization have been initiated
  -> EraGlonassSynchronizedContractVins
  -- ^ A list of VINs already handled by CaRMa (already marked)
  -> NonEmpty SubProgramId
  -- ^ IDs of Era Glonass participant @SubProgram@s
  -> ReaderT SqlBackend m ()

synchronizeContracts nowDay alreadyHandledVins egSubProgramKeys = do
  let subProgramModelName = typeName (Proxy :: Proxy SubProgram) :: Text
  let contractModelName   = typeName (Proxy :: Proxy Contract)   :: Text

  srcLogDebug $ [qms|
    Marking VINs which belong to Era Glonass participant "{subProgramModelName}"
    as handled by us (except those which are already marked as handled by us)
    is initiated.
  |] <> "\n" <> [qmb|
    \  Count of already handled VINs: \
         {length $ fromEraGlonassSynchronizedContractVins alreadyHandledVins}.
    \  Count of active Era Glonass participant "{subProgramModelName}"s: \
         {length egSubProgramKeys}
  |]

  -- Contracts to synchronize
  (contractVINs :: ([ExtrudeContractVinError Text], [(ContractId, EGVin)])) <-
    let
      extract
        :: Entity Contract
        -> ([ExtrudeContractVinError Text], [(ContractId, EGVin)])
        -> ([ExtrudeContractVinError Text], [(ContractId, EGVin)])

      extract e@Entity { entityKey } (errors, succeeded) =
        case extrudeContractVIN e of
             Right x -> (errors, (entityKey, x) : succeeded)
             Left  x -> (x : errors, succeeded)

      contractStillValidFilter
        =   [ ContractValidUntil ==. Nothing ]
        ||. [ ContractValidUntil !=. Nothing
            , ContractValidUntil  <. Just nowDay
            ]

      contractFilter =
        [ ContractIsActive    ==. True
        , ContractSubprogram  <-. (Just <$> toList egSubProgramKeys)
        , ContractVin         !=. Nothing
        , ContractVin        /<-. fromEraGlonassSynchronizedContractVins
                                  alreadyHandledVins
        ]
        <>
        contractStillValidFilter
    in
      selectList contractFilter [] <&> foldr extract ([], [])

  case contractVINs of
       ([], []) -> srcLogDebug [qms|
         It seems that all VINs which belong to Era Glonass participant
         "{subProgramModelName}" are already marked as handled by us,
         so we have nothing to do.
       |]

       (x : xs, []) -> lift $ do
         srcLogDebug [qms|
           All "{contractModelName}"s which belong to Era Glonass participant
           "{subProgramModelName}" are failed to parse, so we have nothing to do
           since we have no valid VINs to bind as handled by us.
         |]

         let errorMessage = [qms|
           All "{contractModelName}"s which belong to Era Glonass participant
           "{subProgramModelName}" are failed to parse with these errors:
           {getErrorMessageOfExtrudingContractVINs $ x :| xs :: Text}
         |]

         srcLogError errorMessage
         saveFailureIncidentInBackground FailureWithoutBody errorMessage

       (errors, x : xs) -> do
         let succeededVINs = x :| xs
         let errorsLen = genericLength errors :: Word

         let failedToParseMsg =
               if errorsLen == minBound
                  then mempty :: Text
                  else [qms| \ (while {errorsLen} "{contractModelName}"s
                                are failed to parse) |]

         srcLogDebug [qms|
           Found {length succeededVINs}\
           {if errorsLen > minBound then " valid" else mempty :: Text} VINs to
           mark as handled by us{failedToParseMsg},
           marking them as handled by us...
         |]

         case errors of
              [] -> pure ()

              e : es -> lift $ do
                let errorMessage = [qms|
                  Some of "{contractModelName}"s which belong to Era Glonass
                  participant "{subProgramModelName}" are failed to parse
                  with these errors:
                  {getErrorMessageOfExtrudingContractVINs $ e :| es :: Text}
                |]

                srcLogError errorMessage
                saveFailureIncidentInBackground FailureWithoutBody errorMessage

         bindVINs succeededVINs


-- | Next step in case there's some VINs which are supposed to be marked as
--   handled by us but not marked yet.
bindVINs
  :: forall m
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadPersistentSql m
   , MonadServantClient m
   , MonadClock m
   , MonadThrow m
   )
  => NonEmpty (ContractId, EGVin)
  -> ReaderT SqlBackend m ()

bindVINs vinsToMark = do
  srcLogDebug [qm| Marking {length vinsToMark} VINs as handled by us... |]

  bindRequest <-
    lift (asks vinSynchronizerContractId) <&> \contractId' ->
      EGBindVehiclesRequest
        { contractId = contractId'
        , vins       = vinsToMark <&> snd
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
           {length vinsToMark} VINs were successfully marked as handled by us to
           Era Glonass service. Now we need to mark them as handled by us on our
           side...
         |]

       EGBindVehiclesResponseFailure { resultCode, description } -> do
         let descriptionText =
               maybe " was not provided"
                     ((": " <>) . fromNonEmptyText)
                     description

         let errorMsg = [qms|
               Marking {length vinsToMark} VINs as handled by us is failed due
               to failure case in the response from EG service, result code is
               {resultCode}, textual failure description{descriptionText}
             |]

         srcLogError [qm| {errorMsg} |]
         throwM $ FailureScenario errorMsg

  srcLogDebug $
    let
      model = typeName (Proxy :: Proxy EraGlonassSynchronizedContract) :: Text
    in [qms|
      Looking for already existing "{model}"s in case some of
      {length vinsToMark} VINs could used to be marked as handled by us
      before (but "unmarked" and now we're marking them again)...
    |]

  currentTime <- lift getCurrentTime

  (vinsLeftToMark :: [(ContractId, EGVin)]) <- do
    previousEGSynchronizedContracts <-
      selectList
        [ EraGlonassSynchronizedContractContract <-. toList (fst <$> vinsToMark)
        ] []

    let model = typeName (Proxy :: Proxy EraGlonassSynchronizedContract) :: Text

    let haveNone = toList vinsToMark <$ srcLogDebug [qms|
          No previous "{model}"s found for {length vinsToMark} VINs
          we're marking as handled by us on our side.
        |]

    let haveSome previouslySynchronized = do
          srcLogDebug [qms|
            Found {length previouslySynchronized} "{model}"s for
            {length vinsToMark} VINs we're marking as handled by us on our side.
            Updating them instead of creating new ones...
          |]

          let hasPrevious = go where
                go = fst ? (`elem` previouslySynchronizedContractIds)

                previouslySynchronizedContractIds =
                  previouslySynchronized <&>
                    entityVal ? eraGlonassSynchronizedContractContract

          let (prevVINsToMark, newVINsToMark) = partition hasPrevious vinsToMark
          let contractModel = typeName (Proxy :: Proxy Contract) :: String

          (xs :: [(EraGlonassSynchronizedContractId, EGVin)]) <-
            forM prevVINsToMark $ \(contractId, vin) -> do
              let found
                    = entityKey <$> find
                    ( entityVal
                    ? eraGlonassSynchronizedContractContract
                    ? (== contractId)
                    ) previouslySynchronized

              case found of
                   Just synchronizedContractId ->
                     pure (synchronizedContractId, vin)

                   Nothing -> throwM $ AssertionFailed [qms|
                     Unexpectedly "{model}" not found by
                     "{contractModel}" id#{fromSqlKey contractId}.
                   |]

          forM_ xs $ \(synchronizedContractId, vin) ->
            update synchronizedContractId
              [ EraGlonassSynchronizedContractVin =. egVinToString vin
              , EraGlonassSynchronizedContractIsHandledByCarma =. True
              , EraGlonassSynchronizedContractLastStatusChangeTime =.
                  Just currentTime
              ]

          pure newVINsToMark

    case previousEGSynchronizedContracts of
         []     -> haveNone
         x : xs -> haveSome $ x :| xs

  case vinsLeftToMark of
       [] -> srcLogDebug [qms|
         No new VINs left to mark as handled by us on our side, it seems all of
         them have been previously marked as handled by us, then "unmarked" and
         now we marked them as handled by us again, patching their previous
         synchronization entities in the database instead of creating new ones.
         We are done for now.
       |]

       xs -> do
         srcLogDebug [qms|
           Got {length xs} new VINs left to mark as handled by us on our side,
           creating new synchronization entities for them...
         |]

         newEntitiesCount <-
           fmap length $ insertMany $ xs <&> \(contractId, vin) ->
             EraGlonassSynchronizedContract
               { eraGlonassSynchronizedContractCtime = currentTime
               , eraGlonassSynchronizedContractContract = contractId
               , eraGlonassSynchronizedContractVin = egVinToString vin
               , eraGlonassSynchronizedContractIsHandledByCarma = True
               , eraGlonassSynchronizedContractLastStatusChangeTime = Nothing
               }

         srcLogDebug [qms|
           Creted {newEntitiesCount} new synchronization entities in the
           database for new VINs marked as handled by us.
         |]
