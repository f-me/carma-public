{-# LANGUAGE OverloadedStrings #-}

-- | Checker combinators.


module Snap.Snaplet.Candibober.Checks

where

import qualified Data.ByteString as B

import Data.Functor

import qualified Data.Map as M

import Snap.Snaplet.Redson.Snapless.Metamodel


------------------------------------------------------------------------------
-- | Datasets provided to Candibober may have several named slots,
-- each containing a commit (think several model instances).
type Dataset = M.Map SlotName Commit


type SlotName = B.ByteString


------------------------------------------------------------------------------
-- | Checker function for condition.
type Checker = Dataset -> CheckResult


------------------------------------------------------------------------------
-- | If condition check explicitly failed or succeeded, Just Bool is
-- returned. If no data was provided, then checkers return Nothing.
type CheckResult = Maybe Bool


------------------------------------------------------------------------------
-- | Checker with logically inverse behaviour.
inverseChecker :: Checker -> Checker
inverseChecker original = (fmap not) . original


------------------------------------------------------------------------------
-- | Combinator which builds checkers acting on the field in the slot
-- of a dataset.
--
-- If the slot is not present in dataset, or the field is not in the
-- commit, Nothing is returned as 'CheckResult'.
fieldChecker :: SlotName 
             -> FieldName 
             -> (FieldValue -> CheckResult)
             -- ^ Checker which acts on the value of the named field
             -- in the slot
             -> Checker
fieldChecker slot field f = \ds -> M.lookup slot ds >>= M.lookup field >>= f


------------------------------------------------------------------------------
-- | Date-based checks
dateCheck :: CheckerArgs -> Ordering -> SlotName -> FieldName -> Checker
dateCheck args LT slot field = inverseChecker $ dateCheck args GT slot field


type CheckerArg = B.ByteString

------------------------------------------------------------------------------
-- | Arguments for checks.
data CheckerArgs = Single CheckerArg
                 | Many [CheckerArg]
                 | None
