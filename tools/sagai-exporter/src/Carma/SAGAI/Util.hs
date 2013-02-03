{-|

  Various string helpers used during export process.

-}

module Carma.SAGAI.Util
    ( padLeft
    , padRight
    , parseTimestamp
    )

where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Data.Text.Encoding

import Data.Time.Clock
import Data.Time.Format
import System.Locale


-- | Return string required to pad input up to provided length. Length
-- is calculated using UTF-8 characters. If input is already not less
-- than required length, return empty string.
genericPad :: Int
           -- ^ Required result length.
           -> Char
           -- ^ Padding symbol.
           -> BS.ByteString
           -- ^ Input string.
           -> BS.ByteString
genericPad padLen pad input =
    if len < padLen
    then (B8.replicate (padLen - len) pad)
    else BS.empty
    where
      len = T.length $ decodeUtf8 input


-- | Pad input using 'genericPad', keeping original string to the right.
padRight :: Int
         -- ^ Minimal string length.
         -> Char
         -- ^ Padding symbol.
         -> BS.ByteString
         -- ^ Input string.
         -> BS.ByteString
padRight padLen pad input = BS.append (genericPad padLen pad input) input


-- | Pad input using 'genericPad', keeping original string to the left.
padLeft :: Int
        -- ^ Minimal string length.
        -> Char
        -- ^ Padding symbol.
        -> BS.ByteString
        -- ^ Input string.
        -> BS.ByteString
padLeft padLen pad input = BS.append input (genericPad padLen pad input)


parseTimestamp :: B8.ByteString -> Maybe UTCTime
parseTimestamp = parseTime defaultTimeLocale "%s" . B8.unpack
