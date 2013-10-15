
module Data.Model.Types where

import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.Aeson.Types as Aeson

import Database.PostgreSQL.Simple.FromRow   (RowParser)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action)
import Data.Dynamic


data Wrap t a = Wrap {unWrap :: a}


data Ident model = Ident {identVal :: Int}
  deriving (Typeable, Eq)

instance FromField (Ident m) where
  fromField f x = Ident `fmap` fromField f x

instance FromJSON (Ident m) where
  parseJSON = fmap Ident . parseJSON

instance ToJSON (Ident m) where
  toJSON (Ident i) = toJSON i

instance ToField (Ident m) where
  toField (Ident i) = toField i


data ModelInfo m = ModelInfo
  { modelName      :: Text
  , tableName      :: Text
  , modelFields    :: [FieldDesc m]
  , modelFieldsMap :: HashMap Text (FieldDesc m)
  }

data FieldDesc m = FieldDesc
  {fd_name       :: Text
  ,fd_desc       :: Text
  ,fd_type       :: TypeRep
  ,fd_parseJSON  :: Value -> Parser Dynamic
  ,fd_toJSON     :: Dynamic -> Value
  ,fd_fromField  :: RowParser Dynamic
  ,fd_toField    :: Dynamic -> Action
  ,fd_coffeeType :: Text
  }
