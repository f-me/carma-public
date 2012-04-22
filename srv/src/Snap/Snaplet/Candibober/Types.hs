{-# LANGUAGE OverloadedStrings #-}

-- | Checker combinators.


module Snap.Snaplet.Candibober.Types
    ( -- * General declarations
      Dataset
    , SlotName
    , Checker
    , FreeChecker
    , CheckResult
    , CheckerArg
    , CheckerArgs(..)
      -- * Checker and argument combinators
    , inverseChecker
    , FieldChecker
    , scopedChecker
    , singleOnly
    )

where

import qualified Data.ByteString.Char8 as B

import qualified Data.Map as M
import Data.Traversable

import Snap.Snaplet.Redson.Snapless.Metamodel


------------------------------------------------------------------------------
-- | Datasets provided to Candibober may have several named slots,
-- each containing a commit (think several model instances).
type Dataset = M.Map SlotName Commit


type SlotName = B.ByteString


------------------------------------------------------------------------------
-- | Checker function for condition.
type Checker = Dataset -> IO CheckResult


------------------------------------------------------------------------------
-- | If condition check explicitly failed or succeeded, Just Bool is
-- returned. If no data was provided, then checkers return Nothing.
type CheckResult = Maybe Bool


type CheckerArg = B.ByteString


------------------------------------------------------------------------------
-- | Arguments for checks.
data CheckerArgs = Single CheckerArg
                 | Many [CheckerArg]


------------------------------------------------------------------------------
-- | Checker which has not been fully bound to check parameters yet.
-- Applying it to arguments yields a checker.
type FreeChecker = CheckerArgs -> Checker


------------------------------------------------------------------------------
-- | Checker with logically inverse behaviour.
inverseChecker :: Checker -> Checker
inverseChecker = (fmap (fmap not) .)


-- | Simplified checker which acts on the value of the named field in the slot
-- instead of whole dataset.
type FieldChecker = FieldValue -> IO Bool


------------------------------------------------------------------------------
-- | Combinator which builds checkers acting on the field in the slot
-- of a dataset.
--
-- If the slot is not present in dataset, or the field is not in the
-- commit, Nothing is returned as 'CheckResult'.
scopedChecker :: SlotName
             -> FieldName
             -> FieldChecker
             -> Checker
scopedChecker slot field f ds =
    traverse f $ M.lookup slot ds >>= M.lookup field


------------------------------------------------------------------------------
-- | Arg combinator which allows to match on @Single CheckerArg@ only.
singleOnly :: CheckerArgs -> (CheckerArgs -> a) -> a
singleOnly s@(Single _) singleF = singleF s
singleOnly (Many _) _ = error $ "Multiple arguments given, expecting one"
