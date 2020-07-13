module Carma.EraGlonass.Types.Helpers.DateTime
     ( parseRFC3339DateTime
     , showRFC3339DateTime
     ) where

import           Data.String (IsString (fromString))
import           Text.Printf (PrintfArg, printf)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime)

import           Control.Applicative (Alternative (empty, (<|>)))


-- | Parses RFC 3339 date-time.
--
-- Returns error message if it's failed to parse.
--
-- Here is few examples of date-times it parses correctly:
--
--   * @parseRFC3339DateTime \"2019-02-28T15:00:49.561Z"@
--   * @parseRFC3339DateTime \"2019-02-28T15:00:49.561+03:00"@
--   * @parseRFC3339DateTime \"2019-02-28T15:00:49.561-02:00"@
--
-- See also https://www.ietf.org/rfc/rfc3339.txt
parseRFC3339DateTime
  :: (IsString s, PrintfArg s, IsString e) => s -> Either e UTCTime

parseRFC3339DateTime = parseTimeResultToEither . go . printf "%s" where
  go x = parse utcRFC3339DateTimeFormat x <|> parse timeOffsetFormat x
  parse = parseTimeM False defaultTimeLocale
  timeOffsetFormat = "%FT%T%Q%z"


showRFC3339DateTime :: IsString s => UTCTime -> s
showRFC3339DateTime =
  fromString . formatTime defaultTimeLocale utcRFC3339DateTimeFormat


utcRFC3339DateTimeFormat :: String
utcRFC3339DateTimeFormat = "%FT%T%QZ"


-- | This data-type exists because "Data.Time.Format.parseTimeM" uses deprecated
--   "Control.Monad.fail" which implemented as throwing exception for
--   @Either e@.
data ParseTimeResult t = ParseTimeFail String | ParseTimeSuccess t

instance Functor ParseTimeResult where
  fmap f (ParseTimeSuccess x) = ParseTimeSuccess $ f x
  fmap _ (ParseTimeFail    e) = ParseTimeFail e

instance Applicative ParseTimeResult where
  pure = ParseTimeSuccess

  ParseTimeSuccess f <*> ParseTimeSuccess x = ParseTimeSuccess $ f x
  ParseTimeFail    e <*> _                  = ParseTimeFail e
  _                  <*> ParseTimeFail    e = ParseTimeFail e

instance MonadFail ParseTimeResult where
  fail = ParseTimeFail

instance Monad ParseTimeResult where
  ParseTimeSuccess x >>= f = f x
  ParseTimeFail    e >>= _ = ParseTimeFail e

instance Alternative ParseTimeResult where
  empty = fail mempty

  ParseTimeFail _ <|> x = x
  x               <|> _ = x

parseTimeResultToEither :: IsString e => ParseTimeResult t -> Either e t
parseTimeResultToEither (ParseTimeSuccess x) = Right x
parseTimeResultToEither (ParseTimeFail    e) = Left $ fromString e
