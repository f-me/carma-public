
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.CoffeeType where

import Data.Text (Text)
import Data.Int (Int16, Int32)
import Data.Vector (Vector)
import Data.Time.Calendar (Day)
import Data.Time.Clock    (UTCTime)

import Data.Model.Types (Ident, Wrap(..))


class CoffeeType t where
  coffeeType :: Wrap t Text

instance CoffeeType Int where
  coffeeType = Wrap "int"

instance CoffeeType Int16 where
  coffeeType = Wrap "int"

instance CoffeeType Int32 where
  coffeeType = Wrap "int"

instance CoffeeType Text where
  coffeeType = Wrap "text"

instance CoffeeType Bool where
  coffeeType = Wrap "Bool"

instance CoffeeType Day where
  coffeeType = Wrap "date"

instance CoffeeType UTCTime where
  coffeeType = Wrap "datetime"

instance CoffeeType t => CoffeeType (Maybe t) where
  coffeeType = Wrap $ unWrap (coffeeType :: Wrap t Text)

instance CoffeeType (Vector t) where
  coffeeType = Wrap "dictionary-set"

instance CoffeeType (Ident x t) where
  coffeeType = Wrap "dictionary"
