{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A helper to wrap persistent key to transform it to/from JSON as string.
module Carma.EraGlonass.Types.PersistentTextKey
     ( PersistentTextKey (..)
     ) where

import           Data.Proxy
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Data.Aeson
import           Data.Swagger
import           Data.Aeson.Types (typeMismatch)
import qualified Data.Attoparsec.Text as P

import           Control.Applicative ((<|>))

import           Database.Persist.Class (ToBackendKey)
import           Database.Persist.Types (Key)
import           Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)


-- | A helper to wrap persistent key to transform it to/from JSON as string.
--
-- It uses @toSqlKey@ and @fromSqlKey@ to obtain integer ID and convert it
-- to/from string (@Text@).
newtype PersistentTextKey record
      = PersistentTextKey { fromPersistentTextKey :: Key record }

instance ToBackendKey SqlBackend record => Eq (PersistentTextKey record) where
  PersistentTextKey modelIdA == PersistentTextKey modelIdB =
    fromSqlKey modelIdA == fromSqlKey modelIdB

instance ToBackendKey SqlBackend record => Show (PersistentTextKey record) where
  show = show . fromSqlKey . fromPersistentTextKey

instance ToSchema (PersistentTextKey record) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToBackendKey SqlBackend record
      => ToJSON (PersistentTextKey record)
         where

  toJSON (PersistentTextKey key) = String [qm|{fromSqlKey key}|]

instance ToBackendKey SqlBackend record
      => FromJSON (PersistentTextKey record)
         where

  parseJSON src@(String x) =
    case P.parseOnly parser x of
         Left  _ -> typeMismatch "PersistentTextKey" src
         Right y -> pure $ PersistentTextKey $ toSqlKey y

    where parser = (negativeParser <|> positiveParser) <* P.endOfInput
          negativeParser = negate <$> (P.char '-' *> P.decimal)
          positiveParser = P.decimal

  parseJSON incorrect = typeMismatch "PersistentTextKey" incorrect
