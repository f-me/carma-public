module Utils.SubscriberId
     ( SubscriberId
     , newSubscriberId
     ) where

import Prelude

import Data.Foldable (foldM)
import Data.List.Lazy (replicate)
import Data.JSDate (now, getTime)

import Effect (Effect)
import Effect.Random (randomInt)


newtype SubscriberId = SubscriberId String
derive instance eqSubscriberId  :: Eq SubscriberId
derive instance ordSubscriberId :: Ord SubscriberId


newSubscriberId :: Effect SubscriberId
newSubscriberId = do
  timeMark <- now <#> getTime >>> show

  randMark <- foldM (\acc x -> x <#> show >>> (acc <> _)) ""
            $ replicate 10
            $ randomInt 0 9

  pure $ SubscriberId $ timeMark <> "_" <> randMark
