{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns, ScopedTypeVariables #-}

module Carma.Model.Types where

import Control.Applicative

import Data.Aeson
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Vector as V
import Data.Vector ((!))

import Data.Time
import Data.Fixed (Pico)

import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..), inQuotes)

import Data.ByteString.Internal (w2c)
import Blaze.ByteString.Builder(Builder, fromByteString)
import Blaze.ByteString.Builder.Char8(fromChar)
import Blaze.Text.Int(integral)

import Data.Aeson () -- (FromJSON, ToJSON)
import Data.Typeable(Typeable)
import Data.Monoid (Monoid, (<>))

import Unsafe.Coerce

import Data.Model
import Data.Model.Types
import Data.Model.CoffeeType

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

instance CoffeeType t => CoffeeType (Interval t) where
  coffeeType = Wrap
    $ "interval-" `T.append` unWrap (coffeeType :: Wrap t Text)
