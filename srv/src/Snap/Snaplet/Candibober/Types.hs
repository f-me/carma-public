{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Candibober combinators: arguments parsing, transformations.


module Snap.Snaplet.Candibober.Types
    ( -- * General declarations
      Dataset
    , SlotName
    , Checker
    , CheckResult
    , CheckerArgs(..)
      -- * Combinators

      -- ** Monadic interface to build concrete checkers from arguments
    , FreeChecker
    , CheckBuilderMonad
    , ArgError(..)

      -- ** Checker combinators
    , inverseChecker
    , FieldChecker
    , scopedChecker

      -- ** Argument combinators
      --
      -- Produce arguments expected by checker combinators
    , singleOnly
    , manyOnly
    , arglessChecker

      -- ** Parsing combinators
      --
      --  Perform early syntax checking on 'CheckerArgs' and feed data
      --  to argument combinators
    , readInteger
    , readSingleString
    , readManyStrings
    , readNone

    -- * Monadic helpers
    , (>>>)
    , (<<<)
    )

where

import Control.Monad
import Control.Monad.Trans.Error

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
-- | Monad used to actually perform checks in.
--
-- We need may need extra database access, thus IO.
type Checking = IO


------------------------------------------------------------------------------
-- | Checker function for condition.
type Checker = Dataset -> Checking CheckResult


------------------------------------------------------------------------------
-- | If condition check explicitly failed or succeeded, Just Bool is
-- returned. If no data was provided, then checkers return Nothing.
type CheckResult = Maybe Bool


type CheckerArg = B.ByteString


------------------------------------------------------------------------------
-- | Arguments for checks.
data CheckerArgs = Single CheckerArg
                 | Many [CheckerArg]
                 | NoArgs



------------------------------------------------------------------------------
-- | Checker which not been fully bound to check parameters yet.
-- Applying it to 'CheckerArgs' yields a checker or fails if arguments
-- are broken.
--
-- Errors are handled through 'CheckBuilderMonad'. All combinators
-- which belong to 'FreeChecker' chain reside in this monad.
--
-- 'FreeChecker' is parametrized over wrapper monad in which
-- combinator chain building is performed.
type FreeChecker m = CheckerArgs -> CheckBuilderMonad m Checker


------------------------------------------------------------------------------
-- | Error-handling monad for use in 'FreeChecker' combinator chain.
--
-- Note that it should not be used to handle errors which occur at
-- check-time.
type CheckBuilderMonad m = ErrorT ArgError m


data ArgError = BadInteger CheckerArg
              -- ^ Could not read integer from stored bytestring
              | BadDate CheckerArg
              -- ^ Could not parse date from stored bytestring
              | UnexpectedMany [CheckerArg]
              -- ^ Many arguments passed when only one is expected
              | UnexpectedSingle CheckerArg
              -- ^ Single argument passed when list is expected
              | NoArguments
              -- ^ No arguments passed while expecting some.
              | ArgError String
              -- ^ Generic argument error with message
              deriving Show


instance Error ArgError where
    strMsg s = ArgError s


------------------------------------------------------------------------------
-- | Checker with logically inverse behaviour.
---
--- 'CheckBuilderMonad' is purely for convenience, this combinator
--- really should be used at the end of 'FreeChecker' chain:
--- 
--- @
--- (inverseChecker . fieldContains "case" "services") <=< readSingleString
--- @
inverseChecker :: Functor m => 
                  CheckBuilderMonad m Checker -> 
                  CheckBuilderMonad m Checker
inverseChecker = fmap (fmap (fmap not) .)


-- | Simplified checker which acts on the value of the named field in the slot
-- instead of whole dataset.
type FieldChecker = FieldValue -> Checking Bool


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
-- | Combinator used when FreeChecker chain is not required (which is
-- the case for checkers without parameters)
arglessChecker :: Monad m => Checker -> FreeChecker m
arglessChecker c = \a -> return $ c


------------------------------------------------------------------------------
-- | Arg combinator which allows to match on @Single CheckerArg@ only.
singleOnly :: Monad m =>
              CheckerArgs 
           -> (CheckerArgs -> CheckBuilderMonad m a) 
           -> CheckBuilderMonad m a
singleOnly s@(Single _) singleF = singleF s
singleOnly (Many e) _ = throwError $ UnexpectedMany e
singleOnly NoArgs _ = throwError NoArguments


------------------------------------------------------------------------------
-- | Arg combinator which allows to match on @Many [CheckerArg]@ only.
manyOnly :: Monad m =>
            CheckerArgs 
         -> (CheckerArgs -> CheckBuilderMonad m a) 
         -> CheckBuilderMonad m a
manyOnly m@(Many _) manyF = manyF m
manyOnly (Single e) _ = throwError $ UnexpectedSingle e
manyOnly NoArgs _ = throwError NoArguments


------------------------------------------------------------------------------
-- | Read single integer argument.
readInteger :: Monad m => CheckerArgs -> CheckBuilderMonad m Integer
readInteger a = singleOnly a $ \(Single s) ->
    case B.readInteger s of
      Just (n, _) -> return n
      _ -> throwError (BadInteger s)


------------------------------------------------------------------------------
-- | Read list of ByteStrings.
readManyStrings :: Monad m => CheckerArgs -> CheckBuilderMonad m [B.ByteString]
readManyStrings a = manyOnly a $ \(Many l) -> return l


------------------------------------------------------------------------------
-- | Read list of ByteStrings.
readSingleString :: Monad m => CheckerArgs -> CheckBuilderMonad m B.ByteString
readSingleString a = singleOnly a $ \(Single l) -> return l


------------------------------------------------------------------------------
-- | Kleisli composition wrapper to maintain monadic error-checking
-- context but do not actually pass any values.
(>>>) f g = (>=>) f $ const g
(<<<) = flip (>>>)

------------------------------------------------------------------------------
-- Check that no arguments were passed for condition.
--
-- Use with '<<<'.
readNone :: Monad m => CheckerArgs -> CheckBuilderMonad m ()
readNone arg = case arg of
                 NoArgs -> return ()
                 Single e -> throwError $ UnexpectedSingle e
                 Many e -> throwError $ UnexpectedMany e
