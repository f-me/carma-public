{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

-- | Syslog logger writer.
--
-- It's a fork of (forked in order to support custom \"ident"):
--   https://www.stackage.org/haddock/lts-9.21/monad-logger-syslog-0.1.4.0/src/System.Log.MonadLogger.Syslog.html
module Carma.Utils.MonadLogger.Syslog
     ( runSyslogLoggingT
     ) where

import           Text.InterpolatedString.QM
import qualified Data.ByteString.Unsafe as BSU

import           Control.Monad.Logger

import           System.Posix.Syslog
import           System.Log.FastLogger (fromLogStr)


runSyslogLoggingT :: String -> LoggingT m a -> m a
runSyslogLoggingT ident =
  (`runLoggingT` formattedSyslogOutput ident defaultLogStr)


-- | Given a "Control.Monad.Logger" log formatter, this writes the log
-- to the syslog,
formattedSyslogOutput
  :: String
  -> (Loc -> LogSource -> LogLevel -> LogStr -> LogStr)
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()

formattedSyslogOutput ident f l s level msg
  = withSyslog ident [DelayedOpen] User
  $ BSU.unsafeUseAsCStringLen (fromLogStr $ f l s level msg)
  $ syslog (Just User) (levelToPriority level)


levelToPriority :: LogLevel -> Priority
levelToPriority LevelDebug = Debug
levelToPriority LevelInfo  = Info
levelToPriority LevelWarn  = Warning
levelToPriority LevelError = Error
levelToPriority (LevelOther level) =
  case level of
    "Emergency" -> Emergency
    "Alert"     -> Alert
    "Critical"  -> Critical
    "Notice"    -> Notice
    _ -> error [qm| unknown log level: {level} |]
