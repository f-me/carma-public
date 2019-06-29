{-# LANGUAGE OverloadedStrings, OverloadedLists, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, InstanceSigs, DeriveGeneric #-}

-- | A module that helps to deal with \"request id" of Era Glonass service.
module Carma.EraGlonass.Types.EGRequestId
     ( EGRequestId
     , fromEGRequestId
     , newEGRequestId
     , egRequestIdParser
     ) where

import           GHC.Generics

import           Data.Proxy
import           Numeric (showHex)
import           Data.Char (digitToInt)
import           Data.Monoid ((<>))
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Digest.Pure.MD5 (md5)
import           Text.InterpolatedString.QM
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.String (IsString (fromString))
import           Data.Swagger
import           Data.Swagger.Declare (Declare)
import           Data.Aeson
import           Data.Aeson.Types hiding (Parser)
import qualified Data.Aeson.Types
import           Data.Attoparsec.ByteString.Char8 hiding (take)

import           Control.Monad.Random.Class (MonadRandom, getRandoms)

import           Database.PostgreSQL.Simple.FromField
                   ( FromField (..)
                   , fromJSONField
                   )
import           Database.PostgreSQL.Simple.ToField
                   ( ToField (..)
                   , toJSONField
                   )
import           Database.Persist.Sql (PersistFieldSql (sqlType))
import           Database.Persist.Types (SqlType (..), PersistValue (..))
import           Database.Persist.Class
                   ( PersistField (toPersistValue, fromPersistValue)
                   , fromPersistValueJSON
                   )

import           Data.Model
import           Data.Model.Types
import           Carma.Monad.Clock
import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Generic.DataType


-- | "EGRequestId" is a free string that looks like:
--   "c94eea91-d647-43d2-af04-109fbb53d8dc".
--
-- It's an UUID.
--
-- Read about UUID here:
--   https://en.wikipedia.org/wiki/Universally_unique_identifier
--
-- It represents unique identity of a \"service for request".
newtype EGRequestId = EGRequestId ByteString deriving (Eq, Show, Generic)

fromEGRequestId :: EGRequestId -> ByteString
fromEGRequestId (EGRequestId x) = x

instance IsString EGRequestId where
  fromString :: forall t. t ~ EGRequestId => String -> t
  fromString x
    = parseOnly egRequestIdParser (fromString x)
    & \case Right y -> y
            Left  e -> error [qms| {typeName (Proxy :: Proxy t) :: String}
                                   "{x}" is incorrect, error: {e} |]

egRequestIdParser :: Parser EGRequestId
egRequestIdParser = go where
  go = f
    <$> count (allDashesParts !! 0) hexDigit <* char '-'
    <*> count (allDashesParts !! 1) hexDigit <* char '-'
    <*> count (allDashesParts !! 2) hexDigit <* char '-'
    <*> count (allDashesParts !! 3) hexDigit <* char '-'
    <*> count (allDashesParts !! 4) hexDigit <* endOfInput

  hexDigit = parser where
    parser = digitToInt <$> satisfy (`elem` chars)
    chars  = ['a'..'f'] <> ['A'..'F'] <> ['0'..'9'] :: [Char]

  f :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> EGRequestId
  f a b c d e = x where
    x = EGRequestId [qm| {g a}-{g b}-{g c}-{g d}-{g e} |]
    g = mconcat . map (flip showHex "") :: [Int] -> String

instance FromJSON EGRequestId where
  parseJSON :: forall t. t ~ EGRequestId => Value -> Data.Aeson.Types.Parser t

  parseJSON v@(String x)
    = parseOnly egRequestIdParser (encodeUtf8 x)
    & \case Left  _ -> typeMismatch (typeName (Proxy :: Proxy t)) v
            Right y -> pure y

  parseJSON invalid = typeMismatch (typeName (Proxy :: Proxy t)) invalid

instance ToJSON EGRequestId where
  toJSON (EGRequestId x) = String $ decodeUtf8 x

instance ToSchema EGRequestId where
  declareNamedSchema
    :: forall proxy t. t ~ EGRequestId
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _
    = pure
    $ NamedSchema (Just $ typeName (Proxy :: Proxy t)) mempty
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

instance PersistField EGRequestId where
  toPersistValue (EGRequestId x) = PersistText $ decodeUtf8 x
  fromPersistValue = fromPersistValueJSON

instance PersistFieldSql EGRequestId where
  sqlType Proxy = SqlString

instance FromField EGRequestId where
  fromField = fromJSONField

instance ToField EGRequestId where
  toField = toJSONField

instance PgTypeable EGRequestId where
  pgTypeOf _ = PgType "text" True

instance DefaultFieldView EGRequestId where
  defaultFieldView f = x where
    fieldToFieldKindProxy :: (m -> FF t nm desc a) -> Proxy a
    fieldToFieldKindProxy _ = Proxy

    x = FieldView
      { fv_name = fieldName f
      , fv_type = "text"
      , fv_canWrite = False
      , fv_meta =
          [ ("label", String $ fieldDesc f)
          , ("app",   String $ fieldKindStr $ fieldToFieldKindProxy f)
          ]
      }


-- | Builds new unique "EGRequestId".
newEGRequestId :: (MonadRandom m, MonadClock m) => m EGRequestId
newEGRequestId = do
  (randomPart  :: [Char]) <- take 128 <$> getRandoms
  (currentTime :: [Char]) <- show <$> getCurrentTime

  let -- | Puts dashes to proper places of hash string
      interleaveWithDashes :: String -> String
      interleaveWithDashes x = go where
        go = uncurry f $ foldl reducer ("", x) $ init allDashesParts
        f a b = a <> ('-' : b)

        reducer (prevAcc, rest) i = (newAcc, b) where
          (a, b) = splitAt i rest
          newAcc = if null prevAcc then a else f prevAcc a

  pure $ EGRequestId
       $ Data.ByteString.Char8.pack
       $ interleaveWithDashes
       $ show $ md5
       $ Data.ByteString.Lazy.Char8.pack
       $ randomPart <> ('|' : currentTime)


-- | Lengths of parts between dashes in UUID of "EGRequestId".
allDashesParts :: [Int]
allDashesParts = [8, 4, 4, 4, 12]
