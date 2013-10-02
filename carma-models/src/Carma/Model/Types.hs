{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns #-}

module Carma.Model.Types where

import Control.Applicative

import Data.Aeson
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Vector as V
import Data.Vector ((!))

import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Format (parseTime)

import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..), inQuotes)

import Data.ByteString.Internal (c2w, w2c)
import Blaze.ByteString.Builder(Builder, fromByteString)
import Blaze.ByteString.Builder.Char8(fromChar)
import Blaze.Text.Int(integral)

import Data.Aeson () -- (FromJSON, ToJSON)
import Data.Typeable(Typeable)
import Data.Monoid (Monoid, (<>))

import Data.Model

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
      parseInterval a =
        Interval <$> parseJSON (a ! 0) <*> parseJSON (a ! 1)
  parseJSON v =
    fail $ "expecting Array, but got: " ++ show v

instance (Typeable t, ToJSON t, Show t) => ToJSON (Interval t) where
  toJSON (Interval begin end) = Array $ V.fromList [toJSON begin, toJSON end]

instance FromField (Interval t) where fromField = undefined
instance ToField   (Interval Day) where
  toField = Plain . inQuotes . dayIntervalToBuilder

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

dayToBuilder :: Day -> Builder
dayToBuilder (toGregorian -> (y,m,d)) = do
    pad4 y <> fromChar '-' <> pad2 m <> fromChar '-' <> pad2 d
