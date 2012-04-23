{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

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

      -- ** Checker combinators must be composable with argument
      -- combinators
    , inverseChecker
    , FieldChecker

      -- ** Argument combinators produce arguments expected by checker
      -- combinators
    , scopedChecker
    , singleOnly

      -- ** Parsing combinators perform early syntax checking on
      -- 'CheckerArgs' and feed data to argument combinators
    , readInteger
    )

where

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
-- | Checker which not been fully bound to check parameters yet.
-- Applying it to arguments yields a checker or fails if arguments are
-- broken.
--
-- Errors are handled through 'CheckBuilderMonad'. All combinators
-- which produce 'FreeChecker' values reside in this monad.
--
-- 'FreeChecker' is parametrized over wrapper monad in which
-- combinator chain building is performed.
type FreeChecker m = CheckerArgs -> CheckBuilderMonad m Checker


------------------------------------------------------------------------------
-- | Error-handling monad for use in 'FreeChecker' combinator chain.
type CheckBuilderMonad m = ErrorT ArgError m


data ArgError = BadInteger B.ByteString
              -- ^ Could not read integer from stored bytestring
              | BadDate B.ByteString
              -- ^ Could not parse date from stored bytestring
              | ArgError String
              -- ^ Generic argument error with message
              | UnexpectedMany
              -- ^ Many arguments passed when only one is expected
              deriving Show


instance Error ArgError where
    strMsg s = ArgError s


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
singleOnly :: Monad m =>
              CheckerArgs 
           -> (CheckerArgs -> CheckBuilderMonad m a) 
           -> CheckBuilderMonad m a
singleOnly s@(Single _) singleF = singleF s
singleOnly (Many _) _ = throwError UnexpectedMany


------------------------------------------------------------------------------
-- | Read single integer argument.
readInteger :: Monad m => CheckerArgs -> CheckBuilderMonad m Integer
readInteger a = singleOnly a $ \(Single s) ->
    case B.readInteger s of
      Just (n, _) -> return n
      _ -> throwError (BadInteger s)
