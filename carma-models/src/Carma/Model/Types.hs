{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving
           , ViewPatterns
           , ScopedTypeVariables
           , RankNTypes
           , DeriveGeneric
           , UndecidableInstances
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
import Data.Int (Int16, Int32)
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.Map as Map

import Data.Time
import Data.Time.Calendar ()
import Data.Fixed (Pico)

import Text.Read (readMaybe)

import Database.PostgreSQL.Simple.FromField (FromField(..)
                                            ,ResultError(..)
                                            ,typename
                                            ,returnError)
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..), inQuotes)

import Data.ByteString.Internal (w2c)
import Blaze.ByteString.Builder(Builder, fromByteString)
import Blaze.ByteString.Builder.Char8(fromChar)
import Blaze.Text.Int(integral)

import Data.Typeable
import Data.Monoid (Monoid, (<>))

import Unsafe.Coerce
import GHC.TypeLits
import GHC.Generics

import Data.Model
import Data.Model.Types
import Carma.Model.LegacyTypes


-- ISO 8601
instance FromJSON Day where
  parseJSON (String s)
    = case parseTime undefined "%Y-%m-%d" $ T.unpack s of
      Just day -> return day
      Nothing  -> fail $ "invalid date format: " ++ show s
  parseJSON v = fail $ "invalid date: " ++ show v

-- ISO 8601
instance ToJSON Day where
  toJSON = String . fromString . show

newtype Model m => Dict m = Dict Text
               deriving (FromField, ToField,
                         FromJSON, ToJSON,
                         Typeable, Monoid, IsString)

data (Typeable t, Show t) => Interval t = Interval t t deriving Typeable

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

instance FromField (Interval t) where fromField = undefined

instance ToField   (Interval UTCTime) where
  toField = Plain . inQuotes . utcTimeIntervalToBuilder

instance ToField   (Interval Day) where
  toField = Plain . inQuotes . dayIntervalToBuilder

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

utcTimeIntervalToBuilder :: Interval UTCTime -> Builder
utcTimeIntervalToBuilder (Interval begin end) =
  let (b, e) = (utcTimeToBuilder begin, utcTimeToBuilder end)
  in fromChar '[' <> b <> fromChar ',' <> e <> fromChar ']'

utcTimeToBuilder :: UTCTime -> Builder
utcTimeToBuilder (UTCTime day time) =
    dayToBuilder day <> fromChar ' '
    <> timeOfDayToBuilder (timeToTimeOfDay time) <> fromByteString "+00"

timeOfDayToBuilder :: TimeOfDay -> Builder
timeOfDayToBuilder (TimeOfDay h m s) = do
    pad2 h <> fromChar ':' <> pad2 m <> fromChar ':' <> showSeconds s

showSeconds :: Pico -> Builder
showSeconds xyz
    | yz == 0   = pad2 x
    | z  == 0   = pad2 x <> fromChar '.' <>  showD6 y
    | otherwise = pad2 x <> fromChar '.' <>  pad6   y <> showD6 z
  where
    -- A kludge to work around the fact that Data.Fixed isn't very fast and
    -- doesn't give me access to the MkFixed constructor.
    (x_,yz) = (unsafeCoerce xyz :: Integer)     `quotRem` 1000000000000
    x = fromIntegral x_ :: Int
    (fromIntegral -> y, fromIntegral -> z) = yz `quotRem` 1000000

dayIntervalToBuilder :: Interval Day -> Builder
dayIntervalToBuilder (Interval begin end) =
  let (b, e) = (dayToBuilder begin, dayToBuilder end)
  in fromChar '[' <> b <> fromChar ',' <> e <> fromChar ']'

-- | p assumes its input is in the range [0..9]
p :: Integral n => n -> Builder
p n = fromChar (w2c (fromIntegral (n + 48)))
{-# INLINE p #-}

-- | pad2 assumes its input is in the range [0..99]
pad2 :: Integral n => n -> Builder
pad2 n = let (a,b) = n `quotRem` 10 in p a <> p b
{-# INLINE pad2 #-}

-- | pad4 assumes its input is positive
pad4 :: (Integral n, Show n) => n -> Builder
pad4 abcd | abcd >= 10000 = integral abcd
          | otherwise     = p a <> p b <> p c <> p d
  where (ab,cd) = abcd `quotRem` 100
        (a,b)   = ab   `quotRem` 10
        (c,d)   = cd   `quotRem` 10
{-# INLINE pad4 #-}

pad6 :: Int -> Builder
pad6 xy = let (x,y) = xy `quotRem` 1000
           in pad3 x <> pad3 y

pad3 :: Int -> Builder
pad3 abc = let (ab,c) = abc `quotRem` 10
               (a,b)  = ab  `quotRem` 10
            in p a <> p b <> p c

showD3 :: Int -> Builder
showD3 abc = case abc `quotRem` 100 of
              (a, 0) -> p a
              (a,bc) -> case bc `quotRem` 10 of
                          (b,0) -> p a <> p b
                          (b,c) -> p a <> p b <> p c

showD6 :: Int -> Builder
showD6 xy = case xy `quotRem` 1000 of
              (x,0) -> showD3 x
              (x,y) -> pad3 x <> showD3 y

dayToBuilder :: Day -> Builder
dayToBuilder (toGregorian -> (y,m,d)) = do
    pad4 y <> fromChar '-' <> pad2 m <> fromChar '-' <> pad2 d


-- default filed view
instance DefaultFieldView t a =>
         DefaultFieldView (Maybe t) a where
  defaultFieldView (_ :: m -> FF (Maybe t) nm desc a)
    = defaultFieldView (undefined :: m -> FF t nm desc a)

instance DefaultFieldView UTCTime "default" where
  defaultFieldView f = (defFieldView f)
    { fv_type = "UTCTime"
    , fv_meta
      = Map.insert "regexp" "datetime"
      $ Map.insert "widget" "datetime"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Bool "default" where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Bool"
    , fv_meta
      = Map.insert "widget" "checkbox"
      $ fv_meta $ defFieldView f
    }


instance DefaultFieldView TInt "default" where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Integer"
    , fv_meta
      = Map.insert "regexp" "number"
      $ Map.insert "widget" "text"
      $ fv_meta $ defFieldView f
    }

instance Model m => DefaultFieldView (IdentList m) "default" where
  defaultFieldView f = (defFieldView f)
    { fv_type = "IdentList"
    , fv_meta
      = Map.insert "model" (Aeson.String $ modelName (modelInfo :: ModelInfo m))
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Double "default" where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Double"
    , fv_meta
      = Map.insert "regexp" "double"
      $ Map.insert "widget" "text"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Int "default" where
  defaultFieldView f = (defFieldView f)
    {fv_type = "int"
    ,fv_meta
      = Map.insert "sqltype" (Aeson.String "integer")
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Int16 "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "int"}

instance DefaultFieldView Int32 "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "int"}

instance DefaultFieldView Text "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "text"}

instance DefaultFieldView Day "default" where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Day"
    , fv_meta
      = Map.insert "regexp" "date"
      $ Map.insert "widget" "date"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView PickerField "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "picker"}

instance DefaultFieldView MapField "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "map"}

instance DefaultFieldView Reference "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "reference"}

instance DefaultFieldView Checkbox "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "checkbox"}

instance DefaultFieldView LegacyDate "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "date"}

instance DefaultFieldView LegacyDatetime "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "datetime"}

instance DefaultFieldView Json "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "json"}

instance DefaultFieldView Aeson.Value "default"  where
  defaultFieldView f = (defFieldView f) {fv_type = "json"}

instance DefaultFieldView Phone "default" where
  defaultFieldView f = (defFieldView f)
    {fv_type = "phone"
    ,fv_meta
      = Map.insert "regexp" "phone"
      $ Map.insert "picker" "callPlease"
      $ fv_meta $ defFieldView f
    }

instance Typeable tag => DefaultFieldView (Ident Int tag) "default" where
  defaultFieldView f = (defFieldView f)
    {fv_type = "dictionary"
    ,fv_meta
      = Map.insert "dictionaryName" (Aeson.String $ typeName (undefined :: tag))
      $ Map.insert "dictionaryType" "ModelDict"
      $ Map.insert "bounded" (Aeson.Bool True)
      $ fv_meta $ defFieldView f
    }

instance Typeable tag => DefaultFieldView (Ident Text tag) "default" where
  defaultFieldView f = (defFieldView f)
    {fv_type = "dictionary"
    ,fv_meta
      = Map.insert "dictionaryName" (Aeson.String $ typeName (undefined :: tag))
      $ Map.insert "bounded" (Aeson.Bool True)
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView (Vector Text) a where
  defaultFieldView f = (defFieldView f)
    {fv_type = "dictionary-text-set"
    ,fv_meta
      = Map.insert "widget" "dictionary-many"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView (Ident Int tag) "default" =>
 DefaultFieldView (Vector (Ident Int tag)) "default" where
  defaultFieldView (_ :: m -> F (Vector (Ident Int tag)) nm desc) =
    let v = defaultFieldView (undefined :: m -> F (Ident Int tag) nm desc)
    in v{fv_type = "dictionary-set-int"
        ,fv_meta = Map.insert "widget" "dictionary-many" $ fv_meta v
        }

instance DefaultFieldView (Ident Text tag) "default" =>
 DefaultFieldView (Vector (Ident Text tag)) "default" where
  defaultFieldView (_ :: m -> F (Vector (Ident Text tag)) nm desc) =
    let v = defaultFieldView (undefined :: m -> F (Ident Text tag) nm desc)
    in v{fv_type = "dictionary-set-text"
        ,fv_meta = Map.insert "widget" "dictionary-many" $ fv_meta v
        }


instance DefaultFieldView (Interval UTCTime) "default" where
  defaultFieldView f = (defFieldView f) {fv_type = "interval-datetime"}

instance DefaultFieldView (Interval Day) "default" where
  defaultFieldView f = (defFieldView f)
    {fv_type = "interval-date"
    ,fv_meta
      = Map.insert "widget" "interval-datetime"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView (Vector t) a => DefaultFieldView (Vector (Maybe t)) a
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

instance DefaultFieldView EventType "default" where
  defaultFieldView f = (defFieldView f)

data UserStateVal = LoggedOut | Ready | Rest | Busy | Dinner | ServiceBreak
                  deriving (Eq, Enum, Bounded, Show, Read, Typeable, Generic)

instance FromJSON UserStateVal
instance ToJSON   UserStateVal

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

instance DefaultFieldView UserStateVal "default" where
  defaultFieldView f = (defFieldView f)
    { fv_type = "dictionary"
    , fv_meta = Map.union (fv_meta (defFieldView f)) $ Map.fromList
                [("dictionaryType", "computedDict")
                ,("dictionaryName", "UserStateVal")
                ,("bounded", "true")
                ]
    }

instance DefaultFieldView (Ident t m) "pk" where
  defaultFieldView f = (defFieldView f)
    { fv_meta = Map.fromList [("readonly", Aeson.Bool True)]
    , fv_type = "Integer"
    , fv_canWrite = False
    }

instance DefaultFieldView t "default"
         => DefaultFieldView t "ephemeral" where
  defaultFieldView (_ :: m -> EF t n d) =
    (defaultFieldView (undefined :: m -> F t n d))
        { fv_meta = Map.fromList [("readonly", Aeson.Bool True)] }

typeName :: forall t . Typeable t => t -> Text
typeName _ = T.pack $ tyConName $ typeRepTyCon $ typeOf (undefined :: t)

defFieldView :: (SingI nm, SingI desc) => (m -> FF t nm desc a) -> FieldView
defFieldView f = FieldView
  {fv_name = fieldName f
  ,fv_type = "undefined"
  ,fv_canWrite = True
  ,fv_meta = Map.fromList
    [("label", Aeson.String $ fieldDesc f)
    ]
  }
