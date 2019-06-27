{-# LANGUAGE ConstraintKinds, FlexibleContexts, DeriveDataTypeable #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Additional types for VIN synchronizer.
module Carma.EraGlonass.VinSynchronizer.Types
     ( type VinSynchronizerMonad

     , type OneOrTwoNonEmptyLists (..)
     , getFirstNonEmptyList
     , getSecondNonEmptyList
     ) where

import           Data.List.NonEmpty (type NonEmpty)

import           Control.Monad.Reader (type MonadReader)
import           Control.Monad.Random.Class (type MonadRandom)
import           Control.Monad.Catch (type MonadThrow)

import           Carma.Monad
import           Carma.EraGlonass.Types.AppContext (type AppContext)


-- | VIN synchronizer monad constraint.
type VinSynchronizerMonad m =
   ( MonadReader AppContext m
   , MonadLoggerBus m

   , MonadClock m -- For creating new @EGRequestId@.
                  -- To calc intervals before synchronizations.

   , MonadDelay m -- To wait before synchronizations.
   , MonadPersistentSql m
   , MonadThrow m
   , MonadRandom m -- For creating new @EGRequestId@.
   , MonadConcurrently m
   , MonadServantClient m
   )


-- | Helper type for VINs unmarking.
data OneOrTwoNonEmptyLists first second
   = FirstNonEmptyList  (NonEmpty first)
   | SecondNonEmptyList (NonEmpty second)
   | BothNonEmptyLists  (NonEmpty first) (NonEmpty second)
     deriving (Show, Eq)


getFirstNonEmptyList :: OneOrTwoNonEmptyLists a b -> Maybe (NonEmpty a)
getFirstNonEmptyList (FirstNonEmptyList  x  ) = Just x
getFirstNonEmptyList (SecondNonEmptyList _  ) = Nothing
getFirstNonEmptyList (BothNonEmptyLists  x _) = Just x

getSecondNonEmptyList :: OneOrTwoNonEmptyLists a b -> Maybe (NonEmpty b)
getSecondNonEmptyList (FirstNonEmptyList  _  ) = Nothing
getSecondNonEmptyList (SecondNonEmptyList x  ) = Just x
getSecondNonEmptyList (BothNonEmptyLists  _ x) = Just x
