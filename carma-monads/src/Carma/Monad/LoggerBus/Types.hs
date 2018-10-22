{-# LANGUAGE QuasiQuotes #-}

module Carma.Monad.LoggerBus.Types
     ( LogMessageType (..)
     , LogMessage (..)
     ) where

import qualified Data.Text as T
import           Text.InterpolatedString.QM

import           Control.Monad.Logger

import           System.Log.FastLogger (fromLogStr)


data LogMessageType
   = LogDebug
   | LogInfo
   | LogWarn
   | LogError
     deriving (Show, Eq)


-- | Bangs to avoid lazy errors when they reach logger thread.
data LogMessage
   = LogMessage !LogMessageType !T.Text
   | LogForward !Loc !LogSource !LogLevel !LogStr

instance Show LogMessage where
  show (LogMessage logType msg) =
    [qm| LogMessage {logType} {show msg} |]
  show (LogForward loc src lvl msg) =
    [qm| LogForward {loc} {src} {lvl} {show $ fromLogStr msg} |]

instance Eq LogMessage where
  (LogMessage logType msg) == (LogMessage logType' msg')
    =  logType == logType'
    && msg     == msg'

  (LogForward loc src lvl msg) == (LogForward loc' src' lvl' msg')
    =  loc == loc'
    && src == src'
    && lvl == lvl'
    && fromLogStr msg == fromLogStr msg'

  _ == _ = False
