{-# LANGUAGE UndecidableInstances, QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

-- | A helper to wrap persistent key to transform it to/from JSON as string.
module Carma.EraGlonass.Types.PersistentTextKey
     ( PersistentTextKey (..)
     ) where

import           Data.Proxy
import           Data.Typeable
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import           Data.Aeson
import           Data.Swagger
import           Data.Aeson.Types (Parser, typeMismatch)
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
        deriving Typeable

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

instance (ToBackendKey SqlBackend record, Typeable record)
      => FromJSON (PersistentTextKey record)
         where

  -- | Type annotation added here to provide type-variable @t@ inside
  -- (for type-safety reasons).
  parseJSON :: forall t. t ~ PersistentTextKey record => Value -> Parser t

  parseJSON src@(String x) =
    case P.parseOnly parser x of
         Left  _ -> typeMismatch (show $ typeRep (Proxy :: Proxy t)) src
         Right y -> pure $ PersistentTextKey $ toSqlKey y

    where parser = (negativeParser <|> positiveParser) <* P.endOfInput
          negativeParser = negate <$> (P.char '-' *> P.decimal)
          positiveParser = P.decimal

  parseJSON x = typeMismatch (show $ typeRep (Proxy :: Proxy t)) x
