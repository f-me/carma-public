{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Helpers for VIN synchronizer.
module Carma.EraGlonass.VinSynchronizer.Helpers
     ( getTimeToWait

     , srcLogDebug
     , srcLogInfo
     , srcLogWarn
     , srcLogError

     , extrudeContractVIN
     , extrudeContractVINs
     ) where

import           Data.Proxy
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Data.Either.Combinators (mapLeft)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (TimeZone, ZonedTime, utcToZonedTime)
import           Data.Time.Format

import           Control.Arrow
import           Control.Monad.Catch (MonadThrow (throwM))
import           Control.Exception (AssertionFailed (AssertionFailed))

import           Database.Persist.Class (persistFieldDef)
import           Database.Persist.Sql (fromSqlKey)

import           Database.Persist.Types
                   ( Entity (..)
                   , FieldDef (fieldHaskell)
                   , HaskellName
                   )

import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Monad.Clock
import           Carma.Monad.LoggerBus.Class
import           Carma.Model.Contract.Persistent
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Types.EGVin

import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types
                   ( EGIntegrationPoint (BindVehicles)
                   )


{-|
Returns hours and minutes to wait before triggering next VIN synchronization.

Minutes already included into hours as fractional part.
-}
getTimeToWait :: forall m. MonadClock m => TimeZone -> m (Float, Word)
getTimeToWait = go . utcToZonedTime where
  go :: (UTCTime -> ZonedTime) -> m (Float, Word)
  go toZonedTime = f . toZonedTime <$> getCurrentTime

  format = formatTime defaultTimeLocale
  readFloat = read :: String -> Float
  split = id &&& id

  f =
    format "%H" &&& format "%M" -- Getting hours and minutes
    >>> readFloat *** readFloat
    >>> second (/ 60) -- Minutes to fractional part of an hour

    >>> -- Sum hours and fractional part of an hour (minutes),
        -- getting hours left to 00:00 (with fractional part),
        -- then split united result.
        arr (uncurry (+) ? (`subtract` 24) ? split)

    >>> -- Getting minutes left (as a remainder apart from hours).
        second ( (properFraction :: Float -> (Int, Float))
               ? snd    -- Only remainder
               ? (* 60) -- Back to real minutes
               ? round
               )


srcLogDebug, srcLogInfo, srcLogWarn, srcLogError
  :: MonadLoggerBus m
  => Text
  -> m ()

srcLogDebug = logDebugS logSrc
srcLogInfo  = logInfoS  logSrc
srcLogWarn  = logWarnS  logSrc
srcLogError = logErrorS logSrc

logSrc :: Text
logSrc = [qm| {BindVehicles} |]


extrudeContractVINs
  :: forall f m
   .
   ( Applicative f
   , Traversable f
   , MonadThrow m
   )
  => f (Entity Contract)
  -> m (f EGVin)

extrudeContractVINs = sequenceA . fmap extrudeContractVIN


extrudeContractVIN
  :: forall t m. (t ~ Contract, MonadThrow m) => Entity t -> m EGVin

extrudeContractVIN = go where
  go = either (throwM . AssertionFailed) pure . parse

  parse (Entity id' Contract { contractVin }) =
    maybe (vinNotSetErr id')
          (mapLeft (parseFailureErr id') . stringToProvedEGVin)
          contractVin

  fieldName :: EntityField t typ -> HaskellName
  fieldName = fieldHaskell . persistFieldDef

  modelName    = typeName (Proxy :: Proxy t) :: String
  idFieldName  = fieldName ContractId
  vinFieldName = fieldName ContractVin

  vinNotSetErr id' = Left [qms|
    {modelName} {idFieldName}#{fromSqlKey id'}
    unexpectedly has not set "{vinFieldName}" field.
  |]

  parseFailureErr id' (msg :: String) = [qms|
    {modelName} {idFieldName}#{fromSqlKey id'}
    unexpectedly has incorrect VIN. Parsing error message: {msg}
  |]
