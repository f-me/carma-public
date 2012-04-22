module Snap.Snaplet.Candibober.Date
    ( -- * Checker combinators
      dateCheck
      -- * Argument combinators
    , yearsAgo
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
-- | Read integer argument into date for that number of years ago.
yearsAgo :: CheckerArgs -> DateArg
yearsAgo a = singleOnly a $ \(Single s) ->
    case B.readInteger s of
      Just (years, _) -> do
        now <- utctDay <$> getCurrentTime
        return $ addGregorianYearsClip (-years) now
      _ -> error $ "Could not read year count from " ++ (B.unpack s)


date :: CheckerArgs -> DateArg
date a = singleOnly a $ \(Single s) ->
    case parseTime defaultTimeLocale targetDateFormat (B.unpack s) of
      Just day -> return day
      _ -> error $ "Could not read date from " ++ (B.unpack s)
