{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- A module that helps to deal with "RequestId" of Era Glonass service
module Carma.EraGlonass.Types.RequestId
     ( RequestId
     , newRequestId
     , requestIdParser
     ) where

import           Numeric (showHex)
import           Data.Char (digitToInt)
import           Data.Monoid ((<>))
import           Data.Function ((&))
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Digest.Pure.MD5 (md5)
import           Text.InterpolatedString.QM
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.String (IsString (fromString))
import           Data.Swagger
import           Data.Aeson
import           Data.Aeson.Types hiding (Parser)
import           Data.Attoparsec.ByteString.Char8 hiding (take)

import           Control.Monad.Random.Class (MonadRandom, getRandoms)

import           Carma.Monad.Clock


-- | "RequestId" is a free string that looks like:
--   "c94eea91-d647-43d2-af04-109fbb53d8dc".
--
-- We also have been told it is an UUID.
-- So I think it IS a free string but usually it is an UUID.
-- Read about UUID here:
--   https://en.wikipedia.org/wiki/Universally_unique_identifier
--
-- Looking at this example we could see it looks like it is just an MD5 hash
-- with some dashes.
--
-- It's part of Era Glonass system, so we don't know what actually it is except
-- it must be unique for each request to Era Glonass.
--
newtype RequestId = RequestId ByteString deriving (Eq, Show)

instance IsString RequestId where
  fromString = RequestId . fromString

requestIdParser :: Parser RequestId
requestIdParser = f
  <$> count (allDashesParts !! 0) hexDigit <* char '-'
  <*> count (allDashesParts !! 1) hexDigit <* char '-'
  <*> count (allDashesParts !! 2) hexDigit <* char '-'
  <*> count (allDashesParts !! 3) hexDigit <* char '-'
  <*> count (allDashesParts !! 4) hexDigit <* endOfInput

  where hexDigit = digitToInt <$> satisfy (`elem` chars)
          where chars = ['a'..'f'] <> ['A'..'F'] <> ['0'..'9']

        f :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> RequestId
        f a b c d e = RequestId [qm| {g a}-{g b}-{g c}-{g d}-{g e} |]
          where g :: [Int] -> String
                g = mconcat . map (flip showHex "")

instance FromJSON RequestId where
  parseJSON v@(String x)
    = parseOnly requestIdParser (encodeUtf8 x)
    & \case Left  _ -> typeMismatch "RequestId" v
            Right y -> pure y

  parseJSON invalid = typeMismatch "RequestId" invalid

instance ToJSON RequestId where
  toJSON (RequestId x) = String $ decodeUtf8 x

instance ToSchema RequestId where
  declareNamedSchema _ = pure $ NamedSchema (Just "RequestId") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaFormat  = Just "UUID"
        , _paramSchemaPattern = Just [qm| ^[0-9A-Za-f]\{{allDashesParts !! 0}}
                                          -[0-9A-Za-f]\{{allDashesParts !! 1}}
                                          -[0-9A-Za-f]\{{allDashesParts !! 2}}
                                          -[0-9A-Za-f]\{{allDashesParts !! 3}}
                                          -[0-9A-Za-f]\{{allDashesParts !! 4}}
                                          $ |]
        }
    }


-- Builds new unique "RequestId".
newRequestId :: (MonadRandom m, MonadClock m) => m RequestId
newRequestId = do
  (randomPart  :: [Char]) <- take 128 <$> getRandoms
  (currentTime :: [Char]) <- show <$> getCurrentTime

  pure $ RequestId
       $ Data.ByteString.Char8.pack
       $ interleaveWithDashes
       $ show $ md5
       $ Data.ByteString.Lazy.Char8.pack
       $ randomPart <> ('|' : currentTime)

  where -- Puts dashes to proper places of hash string
        interleaveWithDashes :: String -> String
        interleaveWithDashes x =
          uncurry f $ foldl reducer ("", x) $ init allDashesParts
          where f a b = a <> ('-' : b)
                reducer (prevAcc, rest) i = (newAcc, b)
                  where (a, b) = splitAt i rest
                        newAcc = if null prevAcc then a else f prevAcc a


-- Lengths of parts between dashes in UUID of "RequestId".
allDashesParts :: [Int]
allDashesParts = [8, 4, 4, 4, 12]
