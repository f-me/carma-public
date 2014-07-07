{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving
           , ViewPatterns
           , ScopedTypeVariables
           , RankNTypes
           , DeriveGeneric
           , GADTs
 #-}

module Carma.Model.Types ( Dict(..)
                         , Interval(..)
                         , TInt
                         , IdentList
                         , EventType(..)
                         , UserStateVal(..)
                         ) where

import Control.Applicative

import Data.Aeson as Aeson
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as B16

import Data.Int (Int16, Int32)

import Data.Serialize as S
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.Map as Map

import Data.Time
import System.Locale (defaultTimeLocale)

import Text.Read (readMaybe)
import Text.Printf

import Database.PostgreSQL.Simple.FromField (FromField(..)
                                            ,ResultError(..)
                                            ,typename
                                            ,returnError)
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..), inQuotes)
import qualified Blaze.ByteString.Builder.Char8 as Builder

import GHC.TypeLits
import GHC.Generics
import Data.Typeable

import Data.Model
import Data.Model.Types
import Carma.Model.LegacyTypes


-- ISO 8601
instance FromJSON Day where
  parseJSON (String s)
    = case parseTime defaultTimeLocale "%Y-%m-%d" $ T.unpack s of
      Just day -> return day
      Nothing  -> fail $ "invalid date format: " ++ show s
  parseJSON v = fail $ "invalid date: " ++ show v

-- ISO 8601
instance ToJSON Day where
  toJSON = String . fromString . show

newtype Dict m = Dict Text
   deriving (FromField, ToField,
             FromJSON, ToJSON,
             Typeable, IsString)

data Interval t = Interval t t deriving Typeable

instance (Typeable t, Show t) => Show (Interval t) where
  show (Interval begin end) = show begin ++ " | " ++ show end

instance (FromJSON t, Typeable t, Show t) => FromJSON (Interval t) where
  parseJSON (Array a)
    | V.length a == 2 = parseInterval a
    | otherwise       = fail $ "array should be 2 elements long"
    where
      parseInterval x =
        Interval <$> parseJSON (x ! 0) <*> parseJSON (x ! 1)
  parseJSON v =
    fail $ "expecting Array, but got: " ++ show v

instance (Typeable t, ToJSON t, Show t) => ToJSON (Interval t) where
  toJSON (Interval begin end) = Array $ V.fromList [toJSON begin, toJSON end]

instance FromField (Interval t) where fromField = undefined -- FIXME: ?

instance ToField   (Interval UTCTime) where
  toField (Interval begin end)
    = Plain $ inQuotes $ Builder.fromString
    $ printf "[%s,%s]"
      (formatTime defaultTimeLocale "%0Y-%m-%d %T+00" begin)
      (formatTime defaultTimeLocale "%0Y-%m-%d %T+00" end)

instance ToField   (Interval Day) where
  toField (Interval begin end)
    = Plain $ inQuotes $ Builder.fromString
    $ printf "[%s,%s]"
      (formatTime defaultTimeLocale "%0Y-%m-d" begin)
      (formatTime defaultTimeLocale "%0Y-%m-d" end)



-- | Int wrapper which instructs CaRMa client to use JSON integers in
-- commits.
--
-- This is the preferred integer type for use with new models until
-- string-wrapped integers are no more used anywhere on the client.
newtype TInt = TInt Int deriving (FromField, ToField,
                                  FromJSON, ToJSON, Typeable)

-- | List of model instance identifiers. Used to accomodate client
-- pull-children behaviour.
data IdentList m = RL (Vector (IdentI m))
                       deriving (Typeable)

instance ToField (IdentList m) where
    toField (RL v) = toField v

instance Typeable m => FromField (IdentList m) where
    fromField f s = RL <$> fromField f s

instance ToJSON (IdentList m) where
    toJSON (RL v) = toJSON v

instance FromJSON (IdentList m) where
    parseJSON v = RL <$> parseJSON v


-- default filed view
instance DefaultFieldView t =>
         DefaultFieldView (Maybe t) where
  defaultFieldView (_ :: m -> FF (Maybe t) nm desc a)
    = defaultFieldView (undefined :: m -> FF t nm desc a)

instance DefaultFieldView UTCTime where
  defaultFieldView f = (defFieldView f)
    { fv_type = "UTCTime"
    , fv_meta
      = Map.insert "regexp" "datetime"
      $ Map.insert "widget" "datetime"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Bool where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Bool"
    , fv_meta
      = Map.insert "widget" "checkbox"
      $ fv_meta $ defFieldView f
    }


instance DefaultFieldView TInt where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Integer"
    , fv_meta
      = Map.insert "regexp" "number"
      $ Map.insert "widget" "text"
      $ fv_meta $ defFieldView f
    }

instance Model m => DefaultFieldView (IdentList m) where
  defaultFieldView f = (defFieldView f)
    { fv_type = "IdentList"
    , fv_meta
      = Map.insert "model" (Aeson.String $ modelName (modelInfo :: ModelInfo m))
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Double where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Double"
    , fv_meta
      = Map.insert "regexp" "double"
      $ Map.insert "widget" "text"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Int where
  defaultFieldView f = (defFieldView f)
    {fv_type = "int"
    ,fv_meta
      = Map.insert "sqltype" (Aeson.String "integer")
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Int16 where
  defaultFieldView f = (defFieldView f) {fv_type = "int"}

instance DefaultFieldView Int32 where
  defaultFieldView f = (defFieldView f) {fv_type = "int"}

instance DefaultFieldView Text where
  defaultFieldView f = (defFieldView f) {fv_type = "text"}

instance DefaultFieldView Day where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Day"
    , fv_meta
      = Map.insert "regexp" "date"
      $ Map.insert "widget" "date"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView PickerField where
  defaultFieldView f = (defFieldView f) {fv_type = "picker"}

instance DefaultFieldView MapField where
  defaultFieldView f = (defFieldView f) {fv_type = "map"}

instance DefaultFieldView Reference where
  defaultFieldView f = (defFieldView f) {fv_type = "reference"}

instance DefaultFieldView Checkbox where
  defaultFieldView f = (defFieldView f) {fv_type = "checkbox"}

instance DefaultFieldView LegacyDate where
  defaultFieldView f = (defFieldView f) {fv_type = "date"}

instance DefaultFieldView LegacyDatetime where
  defaultFieldView f = (defFieldView f) {fv_type = "datetime"}

instance DefaultFieldView JsonAsText where
  defaultFieldView f = (defFieldView f) {fv_type = "JsonAsText"}

instance DefaultFieldView Aeson.Value where
  defaultFieldView f = (defFieldView f) {fv_type = "JSON"}

instance DefaultFieldView Phone where
  defaultFieldView f = (defFieldView f)
    {fv_type = "phone"
    ,fv_meta
      = Map.insert "regexp" "phone"
      $ Map.insert "picker" "callPlease"
      $ fv_meta $ defFieldView f
    }

instance Typeable tag => DefaultFieldView (Ident Int tag) where
  defaultFieldView f = (defFieldView f)
    {fv_type = "dictionary"
    ,fv_meta
      = Map.insert "dictionaryName" (Aeson.String $ typeName (undefined :: tag))
      $ Map.insert "dictionaryType" "ModelDict"
      $ Map.insert "bounded" (Aeson.Bool True)
      $ fv_meta $ defFieldView f
    }

instance Typeable tag => DefaultFieldView (Ident Text tag) where
  defaultFieldView f = (defFieldView f)
    {fv_type = "dictionary"
    ,fv_meta
      = Map.insert "dictionaryName" (Aeson.String $ typeName (undefined :: tag))
      $ Map.insert "bounded" (Aeson.Bool True)
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView (Vector Text) where
  defaultFieldView f = (defFieldView f)
    {fv_type = "dictionary-set-text"
    ,fv_meta
      = Map.insert "widget" "dictionary-many"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView (Ident Int tag) =>
 DefaultFieldView (Vector (Ident Int tag)) where
  defaultFieldView (_ :: m -> FF (Vector (Ident Int tag)) nm desc app) =
    let v = defaultFieldView (undefined :: m -> FF (Ident Int tag) nm desc app)
    in v{fv_type = "dictionary-set-int"
        ,fv_meta = Map.insert "widget" "dictionary-many" $ fv_meta v
        }

instance DefaultFieldView (Ident Text tag) =>
 DefaultFieldView (Vector (Ident Text tag)) where
  defaultFieldView (_ :: m -> FF (Vector (Ident Text tag)) nm desc app) =
    let v = defaultFieldView (undefined :: m -> FF (Ident Text tag) nm desc app)
    in v{fv_type = "dictionary-set-text"
        ,fv_meta = Map.insert "widget" "dictionary-many" $ fv_meta v
        }


instance DefaultFieldView (Interval UTCTime) where
  defaultFieldView f = (defFieldView f) {fv_type = "interval-datetime"}

instance DefaultFieldView (Interval Day) where
  defaultFieldView f = (defFieldView f)
    {fv_type = "interval-date"
    ,fv_meta
      = Map.insert "widget" "interval-datetime"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView (Vector t) => DefaultFieldView (Vector (Maybe t))
  where
  defaultFieldView (_ :: m -> FF (Vector (Maybe t)) nm desc a) =
   defaultFieldView (undefined :: m -> FF (Vector t) nm desc a)

-- | Used to describe event type in Event model
--
-- Represented as "EventType" enum type in postgres
--
-- FIXME: Find a way to check mapping from hs to pg type
data EventType = Login | Logout | Create | Update
               deriving (Eq, Show, Read, Typeable, Generic)

instance FromJSON EventType
instance ToJSON   EventType

instance FromField EventType where
  fromField f mdata = do
    typname <- typename f
    case typname /= "EventType" of
      True  -> returnError Incompatible f ""
      False -> case B.unpack `fmap` mdata of
        Nothing  -> returnError UnexpectedNull f ""
        Just v   -> case readMaybe v of
          Nothing -> returnError ConversionFailed f "mismatched enums"
          Just v' -> return v'

instance ToField EventType where
  toField = toField . show

instance DefaultFieldView EventType where
  defaultFieldView f = (defFieldView f)

data UserStateVal = LoggedOut | Ready | Rest | Busy | Dinner | ServiceBreak
                  deriving (Eq, Enum, Bounded, Show, Read, Typeable, Generic)

instance FromJSON UserStateVal
instance ToJSON   UserStateVal

-- Need this because client send "" instead of null in case of empty
-- which can'd be parsed to 'UserStateVal'
instance FromJSON (Maybe UserStateVal) where
  parseJSON o = do
    case fromJSON o of
      Success v -> return $ Just v
      _err      -> return Nothing

instance ToField UserStateVal where
  toField = toField . show

instance FromField UserStateVal where
  fromField f mdata = do
    typname <- typename f
    case typname /= "UserStateVal" of
      True  -> returnError Incompatible f ""
      False -> case B.unpack `fmap` mdata of
        Nothing  -> returnError UnexpectedNull f ""
        Just v   -> case readMaybe v of
          Nothing -> returnError ConversionFailed f "mismatched enums"
          Just v' -> return v'

instance DefaultFieldView UserStateVal where
  defaultFieldView f = (defFieldView f)
    { fv_type = "dictionary"
    , fv_meta = Map.union (fv_meta (defFieldView f)) $ Map.fromList
                [("dictionaryType", "ComputedDict")
                ,("dictionaryName", "UserStateVal")
                ,("bounded", "true")
                ]
    }


-- | PostGIS coordinates type (longitude and latitude as doubles in
-- WGS 84).
--
-- ToField/FromField instances assume that bytestring data is
-- hex-encoded little-endian WKB for 2D point with SRID=4326.
-- (http://trac.osgeo.org/postgis/browser/trunk/doc/ZMSgeoms.txt)
newtype Coords = Coords (Double, Double)
                 deriving (Show, Typeable)

instance FromField Coords where
    fromField f Nothing = returnError UnexpectedNull f "No coordinates"
    fromField f (Just s) = case S.decode $ fst $ B16.decode s of
                             Left err -> returnError ConversionFailed f err
                             Right v -> return v

instance ToField Coords where
    toField c = Escape $ B16.encode $ S.encode c

instance Serialize Coords where
    get = do
      -- Skip byte order mark
      skip 1
      -- Skip type annotation
      skip 4
      -- Skip SRID value
      skip 4
      -- Read coordinates
      x <- getFloat64le
      y <- getFloat64le
      return $ Coords (x, y)
    put (Coords (x, y)) = do
      -- LE
      putWord8 1
      -- Point with SRID flag
      putWord32le 0x20000001
      -- SRID=4326
      putWord32le 0x000010E6
      -- Write coordinates
      putFloat64le x
      putFloat64le y


typeName :: forall t . Typeable t => t -> Text
typeName _ = T.pack $ tyConName $ typeRepTyCon $ typeOf (undefined :: t)


defFieldView
  :: forall nm desc a m t
  .  (SingI nm, SingI desc, FieldKindSing a)
  => (m -> FF t nm desc a) -> FieldView
defFieldView f = FieldView
  {fv_name = fieldName f
  ,fv_type = "undefined"
  ,fv_canWrite = True
  ,fv_meta = Map.fromList
    [("label",    Aeson.String $ fieldDesc f)
    ,("app",      Aeson.String fieldKind)
    ,("readonly", Aeson.Bool $ fieldKind == "ephemeral")
    ]
  }
  where
    fieldKind = case fieldKindSing :: FieldKindSingleton a of
      FKSDefault   -> "default"
      FKSEphemeral -> "ephemeral"
