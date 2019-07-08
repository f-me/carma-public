{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

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
   = LogMessage !LogSource !LogMessageType !T.Text
   | LogForward !Loc !LogSource !LogLevel !LogStr

instance Show LogMessage where
  show (LogMessage src logType msg) =
    [qm| LogMessage {show src} {logType} {show msg} |]
  show (LogForward src loc lvl msg) =
    [qm| LogForward {src} {loc} {lvl} {show $ fromLogStr msg} |]

instance Eq LogMessage where
  (LogMessage src logType msg) == (LogMessage src' logType' msg')
    =  src     == src'
    && logType == logType'
    && msg     == msg'

  (LogForward loc src lvl msg) == (LogForward loc' src' lvl' msg')
    =  loc == loc'
    && src == src'
    && lvl == lvl'
    && fromLogStr msg == fromLogStr msg'

  _ == _ = False
