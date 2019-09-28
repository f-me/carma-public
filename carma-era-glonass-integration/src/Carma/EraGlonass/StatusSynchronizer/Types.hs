{-# LANGUAGE ExplicitNamespaces #-}

module Carma.EraGlonass.StatusSynchronizer.Types
     ( type BodyParseFailure (..)
     , type FailureScenario (..)
     ) where

import           Data.Aeson

import           Control.Exception (type Exception)


data BodyParseFailure
   = ResponseParseFailure
   { errorMessage :: String
   , responseBody :: Value
   } deriving Show

instance Exception BodyParseFailure


newtype FailureScenario
      = FailureScenario
      { failureMessage :: String
      } deriving Show

instance Exception FailureScenario
