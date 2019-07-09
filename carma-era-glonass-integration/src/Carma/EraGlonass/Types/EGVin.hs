module Carma.EraGlonass.Types.EGVin
     ( Internal.EGVin
     , Internal.egVinParser
     , Internal.textToProvedEGVin
     , stringToProvedEGVin
     , egVinToString
     ) where

import           Data.ByteString.Char8 (unpack)
import           Data.Attoparsec.ByteString.Char8
import           Text.Printf (PrintfArg, printf)
import           Data.String (IsString (fromString))
import           Data.Either.Combinators (mapLeft)

import           Carma.Utils.Operators
import qualified Carma.EraGlonass.Types.EGVin.Internal as Internal


stringToProvedEGVin
  :: (PrintfArg src, IsString src, IsString err)
  => src
  -> Either err Internal.EGVin

stringToProvedEGVin
  = mapLeft fromString
  . parseOnly Internal.egVinParser
  . fromString . printf "%s"


egVinToString :: IsString a => Internal.EGVin -> a
egVinToString = Internal.fromEGVin ? unpack ? fromString
