-- | Date checkers.

module Snap.Snaplet.Candibober.Date
    ( -- * Checker combinators
      compareDate
      -- * Argument combinators
    , yearsAgo
    , monthsAgo
    , daysAgo
    , readDate
      -- * Date formats
    , targetDateFormat
    , commitDateFormat
    )

where

import Control.Monad.Trans.Error

import Data.Functor

import qualified Data.ByteString.Char8 as B

import Data.Time
import System.Locale (defaultTimeLocale)

import Snap.Snaplet.Candibober.Types

import Snap.Snaplet.Redson.Snapless.Metamodel


------------------------------------------------------------------------------
-- | Format used to read dates from check arguments.
targetDateFormat :: String
targetDateFormat = "%d.%m.%Y"


------------------------------------------------------------------------------
-- | Format used to read dates from commit fields.
commitDateFormat :: String
commitDateFormat = "%s"


type DateArg = IO Day


------------------------------------------------------------------------------
-- | Compare day in field value (which must be stored in
-- 'commitDateFormat') and day in check argument.
--
-- Example:
-- @
-- compareDate "service" "car_checkupDate" GT someDay
-- @
-- 
-- Will succeed if date stored in "car_checkupDate" field of "service"
-- slot is greater than someDay.
compareDate :: Monad m =>
               SlotName
            -> FieldName
            -> Ordering
            -- ^ How to compare field value to argument
            -> DateArg
            -- ^ Compare field value with 'Day' returned from this action
            -> CheckBuilderMonad m Checker
compareDate slot field ord day =
    let
        fcheck :: FieldChecker
        fcheck fv = do
          d <- day
          case parseTime defaultTimeLocale commitDateFormat (B.unpack fv) of
            Just cDay -> return $ ord == compare (utctDay cDay) d
            Nothing -> error $
                       "Could not parse date field from dataset " ++
                      (B.unpack slot) ++ "->" ++ (B.unpack field)
    in
      return $ scopedChecker slot field fcheck


------------------------------------------------------------------------------
-- | Read integer into period (as integer), apply given function to
-- the reverse of this period and current date.
dateAgo :: Monad m =>
           (Integer -> Day -> Day)
        -> Integer
        -> CheckBuilderMonad m DateArg
dateAgo adder period = return $ (adder (-period)) <$> utctDay <$> getCurrentTime


------------------------------------------------------------------------------
-- | Read integer argument into date for that number of years ago.
--
-- Use with 'readInteger'.
yearsAgo :: Monad m => Integer -> CheckBuilderMonad m DateArg
yearsAgo = dateAgo addGregorianYearsClip


------------------------------------------------------------------------------
-- | Read integer argument into date for that number of months ago.
monthsAgo :: Monad m => Integer -> CheckBuilderMonad m DateArg
monthsAgo = dateAgo addGregorianMonthsClip

------------------------------------------------------------------------------
-- | Read integer argument into date for that number of months ago.
daysAgo :: Monad m => Integer -> CheckBuilderMonad m DateArg
daysAgo = dateAgo addDays


------------------------------------------------------------------------------
-- | Read formatted date argument using 'targetDateFormat'.
readDate :: Monad m => CheckerArgs -> CheckBuilderMonad m DateArg
readDate a = singleOnly a $ \(Single s) -> do
    case parseTime defaultTimeLocale targetDateFormat (B.unpack s) of
      Just day -> return $ return (day::Day)
      _ -> throwError (BadDate s)
