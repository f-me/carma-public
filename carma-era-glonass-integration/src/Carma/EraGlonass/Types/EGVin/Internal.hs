{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeFamilies, InstanceSigs #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, LambdaCase #-}

-- | @Internal@ module for @EGVin@ type.
--
-- __WARNING!__ This module isn't supposed to be imported from anywhere except
-- test modules (for testing purposes only, to intentionally construct
-- broken/incorrect data to test for failure case).
module Carma.EraGlonass.Types.EGVin.Internal
     ( EGVin (..)
     , egVinParser
     , textToProvedEGVin
     , egVinPosixRegex
     ) where

import           GHC.Generics
import           Generics.Deriving.Eq
import           Generics.Deriving.Show

import           Data.Proxy
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.String (IsString (fromString))
import           Text.InterpolatedString.QM
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger hiding (delete)
import           Data.Swagger.Declare (Declare)
import qualified Data.Attoparsec.ByteString.Char8 as Parsec (Parser)
import           Data.Attoparsec.ByteString.Char8 hiding (Parser)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (toUpper)
import           Data.Monoid ((<>))
import           Data.Either.Combinators (mapLeft)

import           Database.Persist.Class (PersistField (..))
import           Database.Persist.Types (PersistValue (..))

import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType


-- | Alphanumeric of 17 characters excluding O/o, I/i and Q/q.
--
-- An example of a VIN: @JH4TB2H26CC000000@
--
-- See also https://en.wikipedia.org/wiki/Vehicle_identification_number
newtype EGVin = EGVin { fromEGVin :: ByteString } deriving (Show, Generic)

instance GShow EGVin where gshowsPrec = showsPrec
instance GEq   EGVin where geq = (==)

-- | WARNING! This implementation is case-insensitive.
instance Eq EGVin where
  EGVin vinX == EGVin vinY = BS.map toUpper vinX == BS.map toUpper vinY

instance FromJSON EGVin where
  -- | Type annotation added here to provide type-variable @t@ inside
  --   (for type-safety reasons).
  parseJSON :: forall t. t ~ EGVin => Value -> Parser t
  parseJSON (String x) = either fail pure $ textToProvedEGVin x
  parseJSON x = typeMismatch (typeName (Proxy :: Proxy t)) x

instance ToJSON EGVin where
  toJSON = toJSON . decodeUtf8 . fromEGVin

instance ToSchema EGVin where
  -- | Type annotation added here to provide type-variable @t@ inside
  --   (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. t ~ EGVin
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _ = pure
    $ NamedSchema (Just $ typeName (Proxy :: Proxy t)) mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType    = SwaggerString
        , _paramSchemaFormat  = Just "VIN"
        , _paramSchemaPattern = Just egVinPosixRegex
        }
    }

instance PersistField EGVin where
  toPersistValue = PersistByteString . fromEGVin

  fromPersistValue = go where
    parseEGVin = mapLeft fromString . parseOnly egVinParser

    go = \case
      PersistByteString x -> parseEGVin x
      PersistText       x -> parseEGVin $ encodeUtf8 x
      x -> Left [qm| Expected EGVin, received: {x} |]


textToProvedEGVin :: IsString e => Text -> Either e EGVin
textToProvedEGVin = mapLeft fromString . parseOnly egVinParser . encodeUtf8


-- | It isn't exhaustive constraint but we don't have to be that smart here.
--
-- Also we're keeping result case-sensitive here (but not constrainted)
-- to keep value as original as possible.
egVinParser :: Parsec.Parser EGVin
egVinParser = go where
  go  =  EGVin . fromString
     <$> count 17 (satisfy isVinChar)
     <*  endOfInput

  isVinChar = toUpper ? (`elem` vinChars)

  -- | List of allowed chars for a VIN.
  --
  -- Also correct:
  --
  -- @
  -- foldr Data.List.delete ['A'..'Z'] ("IOQ" :: String) <> ['0'..'9']
  -- @
  vinChars :: [Char]
  vinChars = ['A'..'H'] <> ['J'..'N'] <> ('P':['R'..'Z']) <> ['0'..'9']


egVinPosixRegex :: IsString s => s
egVinPosixRegex = "^[A-HJ-NPR-Z0-9]{17}$"
