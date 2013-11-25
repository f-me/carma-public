{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns, ScopedTypeVariables, RankNTypes #-}

module Carma.Model.Types where

import Control.Applicative

import Data.Aeson as Aeson
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read

import Data.Int (Int16, Int32)
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.Map as Map

import Data.Time
import Data.Time.Calendar (Day)
import Data.Fixed (Pico)

import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..), inQuotes)

import Data.ByteString.Internal (w2c)
import Blaze.ByteString.Builder(Builder, fromByteString)
import Blaze.ByteString.Builder.Char8(fromChar)
import Blaze.Text.Int(integral)

import Data.Typeable
import Data.Monoid (Monoid, (<>))

import Unsafe.Coerce
import GHC.TypeLits

import Data.Model
import Data.Model.Types
import Carma.Model.LegacyTypes

instance FromJSON Day where
  parseJSON (String s)
    = case parseTime undefined "%Y-%m-%d" $ T.unpack s of
      Just day -> return day
      Nothing  -> fail $ "invalid date format: " ++ show s
  parseJSON v = fail $ "invalid date: " ++ show v

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

-- | Int wrapper compatible with CaRMa client which uses only strings
-- in field values
newtype TInt = TInt Int deriving (FromField, ToField, DefaultFieldView, Typeable)

instance FromJSON TInt where
    parseJSON (String t) = 
        case decimal t of
          Right (n, _) -> return $ TInt n
          Left _       -> fail "Could not read integer"
    parseJSON _ = empty

instance ToJSON TInt where
    toJSON (TInt n) = String $ T.pack $ show n

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

{-
instance CoffeeType t => CoffeeType (Interval t) where
  coffeeType = Wrap
    $ "interval-" `T.append` unWrap (coffeeType :: Wrap t Text)
-}

-- default filed view
instance DefaultFieldView t => DefaultFieldView (Maybe t) where
  defaultFieldView (f :: m -> F (Maybe t) nm desc)
    = defaultFieldView (undefined :: m -> F t nm desc)


instance DefaultFieldView UTCTime where
  defaultFieldView f = (defFieldView f)
    {fv_type = "datetime"
    }

instance DefaultFieldView Bool where
  defaultFieldView f = (defFieldView f)
    {fv_type = "Bool"
    }

instance DefaultFieldView Int where
  defaultFieldView f = (defFieldView f)
    {fv_type = "int"
    ,fv_meta
      = Map.insert "sqltype" (Aeson.String "integer")
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Int16 where
  defaultFieldView f = (defFieldView f)
    {fv_type = "int"
    }

instance DefaultFieldView Int32 where
  defaultFieldView f = (defFieldView f)
    {fv_type = "int"
    }

instance DefaultFieldView Text where
  defaultFieldView f = (defFieldView f)
    {fv_type = "text"
    }

instance DefaultFieldView Day where
  defaultFieldView f = (defFieldView f)
    {fv_type = "date"
    ,fv_meta
      = Map.insert "regexp" (Aeson.String "date")
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView PickerField where
  defaultFieldView f = (defFieldView f)
    {fv_type = "picker"
    }

instance DefaultFieldView MapField where
  defaultFieldView f = (defFieldView f)
    {fv_type = "map"
    }

instance DefaultFieldView Reference where
  defaultFieldView f = (defFieldView f)
    {fv_type = "reference"
    }

instance DefaultFieldView Checkbox where
  defaultFieldView f = (defFieldView f)
    {fv_type = "checkbox"
    }

instance DefaultFieldView LegacyDate where
  defaultFieldView f = (defFieldView f)
    {fv_type = "date"
    }

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
    {fv_type = "dictionary-many"
    }

instance DefaultFieldView (Ident t tag) =>
 DefaultFieldView (Vector (Ident t tag)) where
  defaultFieldView (f :: m -> F (Vector (Ident t tag)) nm desc) =
    let v = defaultFieldView (undefined :: m -> F (Ident t tag) nm desc)
    in v{fv_type = "dictionary-set"
        ,fv_meta = Map.insert "widget" "dictionary-many" $ fv_meta v
        }

instance DefaultFieldView (Interval UTCTime) where
  defaultFieldView f = (defFieldView f) {fv_type = "interval-datetime"}

instance DefaultFieldView (Interval Day) where
  defaultFieldView f = (defFieldView f) {fv_type = "interval-date"}

instance DefaultFieldView (Vector t) => DefaultFieldView (Vector (Maybe t))
  where
  defaultFieldView (f :: m -> F (Vector (Maybe t)) nm desc) =
   defaultFieldView (undefined :: m -> F (Vector t) nm desc)



typeName :: forall t . Typeable t => t -> Text
typeName _ = T.pack $ tyConName $ typeRepTyCon $ typeOf (undefined :: t)

defFieldView :: (SingI nm, SingI desc) => (m -> F t nm desc) -> FieldView
defFieldView f = FieldView
  {fv_name = fieldName f
  ,fv_type = "undefined"
  ,fv_canWrite = True
  ,fv_meta = Map.fromList
    [("label", Aeson.String $ fieldDesc f)
    ]
  }
