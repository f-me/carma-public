{-# LANGUAGE ExplicitNamespaces #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Additional types for VIN synchronizer.
module Carma.EraGlonass.VinSynchronizer.Types
     ( type OneOrTwoNonEmptyLists (..)
     , getFirstNonEmptyList
     , getSecondNonEmptyList
     , oneOrTwoNonEmptyListsLengths

     , type BodyParseFailure (..)
     , type FailureScenario (..)
     ) where

import           Data.List.NonEmpty (type NonEmpty)
import           Data.Aeson (type Value)

import           Control.Exception (type Exception)


-- | Helper type for VINs unmarking.
data OneOrTwoNonEmptyLists first second
   = FirstNonEmptyList  (NonEmpty first)
   | SecondNonEmptyList (NonEmpty second)
   | BothNonEmptyLists  (NonEmpty first) (NonEmpty second)
     deriving (Show, Eq)


getFirstNonEmptyList :: OneOrTwoNonEmptyLists a b -> Maybe (NonEmpty a)
getFirstNonEmptyList (FirstNonEmptyList  x  ) = Just x
getFirstNonEmptyList (SecondNonEmptyList   _) = Nothing
getFirstNonEmptyList (BothNonEmptyLists  x _) = Just x

getSecondNonEmptyList :: OneOrTwoNonEmptyLists a b -> Maybe (NonEmpty b)
getSecondNonEmptyList (FirstNonEmptyList  _  ) = Nothing
getSecondNonEmptyList (SecondNonEmptyList   x) = Just x
getSecondNonEmptyList (BothNonEmptyLists  _ x) = Just x


oneOrTwoNonEmptyListsLengths :: OneOrTwoNonEmptyLists a b -> (Word, Word)
oneOrTwoNonEmptyListsLengths (FirstNonEmptyList first) =
  ( fromIntegral $ length first
  , minBound
  )
oneOrTwoNonEmptyListsLengths (SecondNonEmptyList second) =
  ( minBound
  , fromIntegral $ length second
  )
oneOrTwoNonEmptyListsLengths (BothNonEmptyLists first second) =
  ( fromIntegral $ length first
  , fromIntegral $ length second
  )


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
