{-# LANGUAGE OverloadedStrings, OverloadedLists, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

-- | A module that helps to deal with "RequestId" of Era Glonass service
module Carma.EraGlonass.Types.RequestId
     ( RequestId
     , fromRequestId
     , newRequestId
     , requestIdParser
     ) where

import           Data.Proxy
import           Data.Typeable
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
newtype RequestId = RequestId ByteString deriving (Eq, Show, Typeable)

fromRequestId :: RequestId -> ByteString
fromRequestId (RequestId x) = x

instance IsString RequestId where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  fromString :: forall t. t ~ RequestId => String -> t
  fromString x
    = parseOnly requestIdParser (fromString x)
    & \case Right y -> y
            Left  e -> error [qms| {typeRep (Proxy :: Proxy t)}
                                   "{x}" is incorrect, error: {e} |]

requestIdParser :: Parser RequestId
requestIdParser = f
  <$> count (allDashesParts !! 0) hexDigit <* char '-'
  <*> count (allDashesParts !! 1) hexDigit <* char '-'
  <*> count (allDashesParts !! 2) hexDigit <* char '-'
  <*> count (allDashesParts !! 3) hexDigit <* char '-'
  <*> count (allDashesParts !! 4) hexDigit <* endOfInput

  where hexDigit = digitToInt <$> satisfy (`elem` chars)
          where chars = ['a'..'f'] <> ['A'..'F'] <> ['0'..'9'] :: [Char]

        f :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> RequestId
        f a b c d e = RequestId [qm| {g a}-{g b}-{g c}-{g d}-{g e} |]
          where g :: [Int] -> String
                g = mconcat . map (flip showHex "")

instance FromJSON RequestId where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ RequestId => Value -> Data.Aeson.Types.Parser t

  parseJSON v@(String x)
    = parseOnly requestIdParser (encodeUtf8 x)
    & \case Left  _ -> typeMismatch (show $ typeRep (Proxy :: Proxy t)) v
            Right y -> pure y

  parseJSON invalid = typeMismatch (show $ typeRep (Proxy :: Proxy t)) invalid

instance ToJSON RequestId where
  toJSON (RequestId x) = String $ decodeUtf8 x

instance ToSchema RequestId where
  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  declareNamedSchema
    :: forall proxy t. t ~ RequestId
    => proxy t
    -> Declare (Definitions Schema) NamedSchema

  declareNamedSchema _
    = pure
    $ NamedSchema (Just [qm|{typeRep (Proxy :: Proxy t)}|]) mempty
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

instance PersistField RequestId where
  toPersistValue (RequestId x) = PersistText $ decodeUtf8 x
  fromPersistValue = fromPersistValueJSON

instance PersistFieldSql RequestId where
  sqlType Proxy = SqlString

instance FromField RequestId where
  fromField = fromJSONField

instance ToField RequestId where
  toField = toJSONField

instance PgTypeable RequestId where
  pgTypeOf _ = PgType "text" True

instance DefaultFieldView RequestId where
  defaultFieldView f
    = FieldView
    { fv_name = fieldName f
    , fv_type = "text"
    , fv_canWrite = False
    , fv_meta =
        [ ("label", String $ fieldDesc f)
        , ("app",   String $ fieldKindStr $ fieldToFieldKindProxy f)
        ]
    } where fieldToFieldKindProxy :: (m -> FF t nm desc a) -> Proxy a
            fieldToFieldKindProxy _ = Proxy


-- | Builds new unique "RequestId".
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


-- | Lengths of parts between dashes in UUID of "RequestId".
allDashesParts :: [Int]
allDashesParts = [8, 4, 4, 4, 12]
