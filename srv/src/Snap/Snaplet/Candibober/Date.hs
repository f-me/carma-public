module Snap.Snaplet.Candibober.Date
    ( -- * Checker combinators
      dateCheck
      -- * Argument combinators
    , yearsAgo
    , monthsAgo
    , date
    )

where

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


dateCheck :: SlotName -> FieldName -> Ordering -> DateArg -> Checker
dateCheck slot field LT day = inverseChecker $ dateCheck slot field GT day
dateCheck slot field GT day =
    let
        -- | Compare day in check argument and day in field value
        fcheck :: FieldChecker
        fcheck fv = do
          d <- day
          case parseTime defaultTimeLocale commitDateFormat (B.unpack fv) of
            Just cDay -> return $ d <= (utctDay cDay)
            Nothing -> error $
                       "Could not parse date field from dataset " ++
                       (B.unpack slot) ++ "->" ++ (B.unpack field)
    in
      scopedChecker slot field fcheck


------------------------------------------------------------------------------
-- | Read integer argument into period (as integer), apply given
-- function to the reverse of this period and current date.
dateAgo :: (Integer -> Day -> Day) -> CheckerArgs -> DateArg
dateAgo adder a = singleOnly a $ \(Single s) ->
    case B.readInteger s of
      Just (period, _) -> do
        now <- utctDay <$> getCurrentTime
        return $ adder (-period) now
      _ -> error $ "Could not read period from " ++ (B.unpack s)


------------------------------------------------------------------------------
-- | Read integer argument into date for that number of years ago.
yearsAgo = dateAgo addGregorianYearsClip


------------------------------------------------------------------------------
-- | Read integer argument into date for that number of months ago.
monthsAgo = dateAgo addGregorianMonthsClip


------------------------------------------------------------------------------
-- | Read formatted date argument using 'targetDateFormat'.
date :: CheckerArgs -> DateArg
date a = singleOnly a $ \(Single s) ->
    case parseTime defaultTimeLocale targetDateFormat (B.unpack s) of
      Just day -> return day
      _ -> error $ "Could not read date from " ++ (B.unpack s)
