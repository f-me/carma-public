{-# LANGUAGE OverloadedStrings, QuasiQuotes, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}

-- | Special additional types for Era Glonass integration models.
module Carma.EraGlonass.Model.Types
     ( PgArray (..)
     , BiggestPgArrayItem (..)
     ) where

import           Data.Proxy
import           Data.Semigroup ((<>))
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.String (fromString)
import           Data.ByteString (ByteString, intercalate)
import           Data.Text (Text)
import           Text.InterpolatedString.QM (qms)
import           Data.Aeson

import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Sql as ESql
import           Database.Persist.Sql (PersistFieldSql (..))
import           Database.Persist.Types (SqlType (SqlOther), PersistValue (..))

import           Database.Persist.Class
                   ( PersistField (..)
                   , PersistEntity (..)
                   , EntityField
                   )

import           Carma.Utils.Operators


newtype PgArray a
      = PgArray [a]
        deriving (Eq, Show, Monoid, Functor, Applicative)

instance ToJSON a => ToJSON (PgArray a) where
  toJSON (PgArray x) = toJSON x

instance FromJSON a => FromJSON (PgArray a) where
  parseJSON x = PgArray <$> parseJSON x

instance ( ToPostgresArrayItem a
         , FromPersistList a
         ) => PersistField (PgArray a) where

  toPersistValue (PgArray list) = go where
    go = PersistDbSpecific $ "{" <> serialized <> "}"
    serialized = intercalate "," $ toPostgresArrayItem <$> list

  fromPersistValue (PersistList list) = PgArray <$> fromPersistList list
  fromPersistValue x = Left [qms| PersistField (PgArray UTCTime):
                                  Unexpected PersistValue: {x}
                                  (it's supposed to be PersistList) |]

instance PersistFieldSql (PgArray UTCTime) where
  sqlType Proxy = SqlOther $ "TIMESTAMP WITH TIME ZONE []"

class ToPostgresArrayItem a where
  toPostgresArrayItem :: a -> ByteString

instance ToPostgresArrayItem UTCTime where
  toPostgresArrayItem =
    fromString . formatTime defaultTimeLocale "'%Y-%m-%d %H:%M:%S'"

class FromPersistList a where
  fromPersistList :: [PersistValue] -> Either Text [a]

instance FromPersistList UTCTime where
  fromPersistList list = sequence $ list <&> \case
    PersistUTCTime t -> Right t
    x -> Left [qms| FromPersistList UTCTime:
                    Unexpected PersistValue: {x}
                    (it's supposed to be PersistUTCTime) |]

class BiggestPgArrayItem typ where
  -- | Instances implemented in a hacky way,
  --   because constructors of "E.SqlExpr" aren't reachable.
  biggestPgArrayItem
    :: (PersistEntity val, PersistField typ)
    => E.SqlExpr (E.Entity val)
    -> EntityField val typ
    -> E.SqlExpr (E.Value typ)

instance BiggestPgArrayItem (PgArray a) where
  entity `biggestPgArrayItem` field =
    ESql.unsafeSqlBinOp " as x "
      (ESql.unsafeSqlFunction "select unnest" (entity E.^. field))
      (ESql.unsafeSqlValue "order by x desc limit 1")

instance BiggestPgArrayItem (Maybe (PgArray a)) where
  entity `biggestPgArrayItem` field =
    ESql.unsafeSqlBinOp " as x "
      (ESql.unsafeSqlFunction "select unnest" (entity E.^. field))
      (ESql.unsafeSqlValue "order by x desc limit 1")
