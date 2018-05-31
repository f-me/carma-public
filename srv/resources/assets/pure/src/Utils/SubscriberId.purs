module Utils.SubscriberId
     ( SubscriberId
     , newSubscriberId
     ) where

import Prelude

import Data.Foldable (foldM)
import Data.List.Lazy (replicate)
import Data.JSDate (now, getTime)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM, randomInt)


newtype SubscriberId = SubscriberId String
derive instance eqSubscriberId  :: Eq SubscriberId
derive instance ordSubscriberId :: Ord SubscriberId


newSubscriberId
  :: forall eff . Eff (now :: NOW, random :: RANDOM | eff) SubscriberId

newSubscriberId = do
  timeMark <- now <#> getTime >>> show

  randMark <- foldM (\acc x -> x <#> show >>> (acc <> _)) ""
            $ replicate 10
            $ randomInt 0 9

  pure $ SubscriberId $ timeMark <> "_" <> randMark
