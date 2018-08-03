module Carma.EraGlonass.Test.Helpers
     ( findSubstring
     ) where

import           Data.Text (Text)
import qualified Data.Attoparsec.Text as ParsecText

import           Control.Applicative ((<|>))


findSubstring :: Text -> ParsecText.Parser Text
findSubstring str =
  ParsecText.try (ParsecText.string str)
    <|> (ParsecText.anyChar *> findSubstring str)
