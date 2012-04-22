{-# LANGUAGE OverloadedStrings #-}

-- | Checker combinators.


module Snap.Snaplet.Candibober.Checks

where

import Control.Monad
import Data.Functor

import qualified Data.ByteString.Char8 as B

import qualified Data.Map as M
import Data.Traversable

import Data.Time
import System.Locale (defaultTimeLocale)

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
-- | Checker with logically inverse behaviour.
inverseChecker :: Checker -> Checker
inverseChecker = (fmap (fmap not) .)


-- |Checker which acts on the value of the named field in the slot
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
-- | Format used to read dates from check arguments.
parseDateFormat :: String
parseDateFormat = "%d.%m.%Y"


------------------------------------------------------------------------------
-- | Format used to read dates from commit fields.
commitDateFormat :: String
commitDateFormat = "%s"


type DateArg = IO Day


------------------------------------------------------------------------------
-- | Date-based checks
dateCheck :: SlotName -> FieldName -> Ordering -> IO Day -> Checker
dateCheck slot field LT day = inverseChecker $ dateCheck slot field GT day
dateCheck slot field GT day =
    let
        -- | Compare day in check argument and day in field value
        fcheck :: FieldChecker
        fcheck fv = do
          d <- day
          let cValue = parseTime defaultTimeLocale commitDateFormat (B.unpack fv)
          case cValue of
            Just cDay -> return $ d <= (utctDay cDay)
    in
      scopedChecker slot field fcheck


------------------------------------------------------------------------------
-- | Read single integer argument into date for that number of years
-- ago.
yearsAgo :: CheckerArgs -> DateArg
yearsAgo (Single s) = 
    case B.readInteger s of
      Just (years, _) -> do
        now <- utctDay <$> getCurrentTime
        return $ addGregorianYearsClip (-years) now
      _ -> error $ "Could not read year count from " ++ (B.unpack s)
yearsAgo (Many _) = error $ "Multiple year periods given, expecting one"
