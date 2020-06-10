{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeFamilies, FlexibleContexts #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Helpers for Status Synchronizer.
module Carma.EraGlonass.StatusSynchronizer.Helpers
     ( srcLogDebug
     , srcLogInfo
     , srcLogWarn
     , srcLogError

     , saveFailureIncidentInBackground
     ) where

import           Data.Proxy
import           Data.Text (Text)

import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Catch (MonadCatch)

import           Carma.Monad
import           Carma.Monad.LoggerBus.Class
import           Carma.Utils.TypeSafe.TypeFamilies (OneOf)
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent
import           Carma.EraGlonass.Helpers
import           Carma.EraGlonass.Types.AppContext (AppContext (..))

import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( EGIntegrationPoint
                       ( ChangeProcessingStatus
                       , ChangeRequestStatus
                       )
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
logSrc = "StatusSynchronizer"


saveFailureIncidentInBackground
  :: forall m bodyType integrationPoint
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadCatch m
   , MonadClock m
   , MonadThread m
   , MonadPersistentSql m
   , MonadSTM m
   , OneOf bodyType '[ 'FailureNoBodyType, 'FailureResponseBodyType ]
   , OneOf integrationPoint '[ 'ChangeProcessingStatus, 'ChangeRequestStatus ]
   , TypeToTermIntegrationPoint integrationPoint
   , GetFailureBodyType bodyType
   )
  => Proxy (integrationPoint :: EGIntegrationPoint)
  -> FailureBody bodyType
  -> Text
  -> m ()

saveFailureIncidentInBackground integrationPoint failureBody =
  reportToHouston failureBody $ typeToTermIntegrationPoint integrationPoint


class TypeToTermIntegrationPoint (a :: EGIntegrationPoint) where
  typeToTermIntegrationPoint :: Proxy a -> EGIntegrationPoint

instance TypeToTermIntegrationPoint 'ChangeProcessingStatus where
  typeToTermIntegrationPoint Proxy = ChangeProcessingStatus

instance TypeToTermIntegrationPoint 'ChangeRequestStatus where
  typeToTermIntegrationPoint Proxy = ChangeRequestStatus
