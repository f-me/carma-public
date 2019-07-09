{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Carma.EraGlonass.Server.ReceiveRequestForServiceRequest.Helpers
     ( srcLogDebug
     , srcLogInfo
     , srcLogWarn
     , srcLogError

     , logSrc
     ) where

import           Data.Text (Text)
import           Text.InterpolatedString.QM

import           Control.Monad.Logger (LogSource)

import           Carma.Monad.LoggerBus.Class

import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( EGIntegrationPoint (RequestForService)
                   )


srcLogDebug, srcLogInfo, srcLogWarn, srcLogError
  :: MonadLoggerBus m
  => Text
  -> m ()

srcLogDebug = logDebugS logSrc
srcLogInfo  = logInfoS  logSrc
srcLogWarn  = logWarnS  logSrc
srcLogError = logErrorS logSrc

logSrc :: LogSource
logSrc = [qm| {RequestForService} |]
