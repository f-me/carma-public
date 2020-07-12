{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, DataKinds #-}
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
     , ExtrudeContractVinError (..)
     , getErrorMessageOfExtrudingContractVINs

     , saveFailureIncidentInBackground
     ) where

import           Data.Proxy
import           Data.Typeable (Typeable)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Data.String (IsString (fromString))
import           Data.Time.LocalTime (TimeZone, ZonedTime, utcToZonedTime)
import           Data.Time.Format

import           Control.Arrow
import           Control.Applicative ((<|>))
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Except (MonadError (throwError))
import           Control.Exception (Exception (displayException))

import           Database.Persist.Class (persistFieldDef)
import           Database.Persist.Sql (fromSqlKey)

import           Database.Persist.Types
                   ( Entity (..)
                   , FieldDef (fieldHaskell)
                   , HaskellName (unHaskellName)
                   )

import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Utils.TypeSafe.TypeFamilies (OneOf)
import           Carma.Monad
import           Carma.Monad.LoggerBus.Class
import           Carma.Model.Contract.Persistent
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent (MonadPersistentSql)
import           Carma.EraGlonass.Helpers
import           Carma.EraGlonass.Types.AppContext (AppContext)
import           Carma.EraGlonass.Types.EGVin

import           Carma.EraGlonass.Types.EGIntegrationPoint
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


-- | Helper type for "getErrorMessageOfExtrudingContractVINs" function.
data ExtrudeContractVinErrorMessage parsingErrMsg
   = ExtrudeContractVinErrorMessage
   { vinFieldIsNotSetIds :: S.Set ContractId
   , incorrectVinValue   :: M.Map parsingErrMsg (S.Set ContractId)
   } deriving (Show, Eq)

getErrorMessageOfExtrudingContractVINs
  :: forall s parsingErrMsg t
   .
   ( IsString s
   , Monoid s
   , Ord s
   , ShowQ parsingErrMsg
   , IsString parsingErrMsg
   , Ord parsingErrMsg
   , t ~ Contract
   )
  => NonEmpty (ExtrudeContractVinError parsingErrMsg)
  -> s

getErrorMessageOfExtrudingContractVINs list = go where
  go = [qm|
    Failed to extract VINs from some of "{modelName}"s.
    { let xs = vinFieldIsNotSetIds folded
       in if xs /= mempty
             then " " <> notSetMsg xs
             else mempty :: s
    }
    { let xs = incorrectVinValue folded
       in if xs /= mempty
             then " " <> incorrectMsg xs
             else mempty :: s
    }
  |]

  modelName = typeName (Proxy :: Proxy t) :: String

  fieldName :: EntityField t typ -> Text
  fieldName = unHaskellName . fieldHaskell . persistFieldDef

  renderIds ids = x :: s where
    x = showId `S.map` ids & foldl reduceIdToString mempty
    showId = fromString . show . fromSqlKey
    reduceIdToString acc x' = if acc == mempty then x' else acc <> ", " <> x'

  renderIncorrect idsPerMsg = x :: s where
    x = idsPerMsg <&> renderIds & M.foldlWithKey reduceToString mempty
    showErrMsg msg = [qm|{msg :: parsingErrMsg}|] :: s

    reduceToString acc msg ids
      = (if acc == mempty then id else \x' -> acc <> "; " <> x')
      $ showErrMsg msg <> ids

  notSetMsg xs = [qms|
    Some of "{modelName}"s have empty value
    of "{fieldName ContractVin}" field,
    here are IDs of those "{modelName}"s: {renderIds xs}.
  |]

  incorrectMsg xs = [qms|
    Some of "{modelName}"s have incorrect value
    of "{fieldName ContractVin}" field, here are errors
    (error messages and for each one a list of IDs which failed to parse
    with that error message): {renderIncorrect xs}
  |]

  folded = foldl reducer (ExtrudeContractVinErrorMessage mempty mempty) list

  reducer
    acc@ExtrudeContractVinErrorMessage { vinFieldIsNotSetIds = ids }
    (VinFieldIsNotSet id') = acc { vinFieldIsNotSetIds = id' `S.insert` ids }
  reducer
    acc@ExtrudeContractVinErrorMessage { incorrectVinValue = msgMap }
    (VinValueIsIncorrect id' msg) = x where
      x = acc { incorrectVinValue = f msgMap }
      f = M.alter ((S.insert id' <$>) ? (<|> Just (S.singleton id'))) msg


-- | Plural version of "extrudeContractVIN".
extrudeContractVINs
  :: (ShowQ parsingErrMsg, IsString parsingErrMsg)
  => NonEmpty (Entity Contract)
  -> NonEmpty (Either (ExtrudeContractVinError parsingErrMsg) EGVin)

extrudeContractVINs = fmap extrudeContractVIN


extrudeContractVIN
  :: forall t m parsingErrMsg
   .
   ( t ~ Contract
   , ShowQ parsingErrMsg
   , IsString parsingErrMsg
   , MonadError (ExtrudeContractVinError parsingErrMsg) m
   )
  => Entity t
  -> m EGVin

extrudeContractVIN (Entity id' Contract { contractVin })
  = flip (maybe $ throwError $ VinFieldIsNotSet id') contractVin
  $ stringToProvedEGVin ? either (throwError . VinValueIsIncorrect id') pure

data ExtrudeContractVinError parsingErrMsg
   = VinFieldIsNotSet    ContractId
   | VinValueIsIncorrect ContractId parsingErrMsg
     deriving (Eq, Show, Typeable)

instance ( Typeable parsingErrMsg
         , Show parsingErrMsg
         , ShowQ parsingErrMsg
         , t ~ Contract
         ) => Exception (ExtrudeContractVinError parsingErrMsg) where

  displayException (VinFieldIsNotSet id') = [qms|
    {typeName (Proxy :: Proxy t) :: String} {
      fieldHaskell $ persistFieldDef (ContractId :: EntityField t ContractId)
    }#{fromSqlKey id'}
    unexpectedly has not set "{
      fieldHaskell $ persistFieldDef (ContractVin :: EntityField t (Maybe Text))
    }" field.
  |]

  displayException (VinValueIsIncorrect id' msg) = [qms|
    {typeName (Proxy :: Proxy t) :: String} {
      fieldHaskell $ persistFieldDef (ContractId :: EntityField t ContractId)
    }#{fromSqlKey id'}
    unexpectedly has incorrect VIN. Parsing error message: {msg}
  |]


saveFailureIncidentInBackground
  :: forall m bodyType
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadCatch m
   , MonadClock m
   , MonadThread m
   , MonadPersistentSql m
   , MonadSTM m
   , OneOf bodyType '[ 'FailureNoBodyType, 'FailureResponseBodyType ]
   , GetFailureBodyType bodyType
   )
  => FailureBody bodyType
  -> Text
  -> m ()

saveFailureIncidentInBackground failureBody =
  reportToHouston failureBody BindVehicles
