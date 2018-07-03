{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Carma.EraGlonass.Test.Utils.RequestId
     ( spec
     ) where

import           Test.Hspec

import           Data.Function ((&))
import           Data.Time.Calendar
import           Data.Time.Clock hiding (getCurrentTime)
import           Text.InterpolatedString.QM

import           Control.Monad (ap)
import           Control.Monad.Random.Class

import           System.Random

import           Carma.EraGlonass.RequestId
import           Carma.Monad.Clock


-- Value contatiner with action mark accumulator
data TestState a = TestState [String] a deriving (Show, Eq)

instance Functor TestState where
  fmap f (TestState s a) = TestState s $ f a

instance Applicative TestState where
  pure a = TestState [] a
  (<*>) = ap

instance Monad TestState where
  TestState s1 a1 >>= f = f a1 & \(TestState s2 a2) -> TestState (s1 ++ s2) a2


instance {-# OVERLAPS #-} MonadClock TestState where
  getCurrentTime = TestState ["MonadClock.getCurrentTime"] $
    -- "Show" result of it would be "1858-11-19 00:00:03 UTC"
    UTCTime (ModifiedJulianDay 2) (secondsToDiffTime 3)

data TestRandomGen = TestRandomGen

instance RandomGen TestRandomGen where
  next TestRandomGen = (minBound + 42, TestRandomGen) -- for "Char" it's '*'
  split TestRandomGen = (TestRandomGen, TestRandomGen)

instance MonadRandom TestState where
  -- these haven't used
  getRandomR  _ = TestState ["MonadRandom.getRandomR"]  undefined
  getRandom     = TestState ["MonadRandom.getRandom"]   undefined
  getRandomRs _ = TestState ["MonadRandom.getRandomRs"] undefined

  -- getRandoms :: Random a => m [a]
  getRandoms = TestState ["MonadRandom.getRandoms"] $ randoms TestRandomGen


spec :: Spec
spec = do
  describe "Entropy is enough" $ do
    it "Gets current time" $
      state `shouldSatisfy` elem "MonadClock.getCurrentTime"

    it "Gets random number" $
      state `shouldSatisfy` elem "MonadRandom.getRandoms"

    it "Does not touche other random functions" $ do
      state `shouldNotSatisfy` elem "MonadRandom.getRandomR"
      state `shouldNotSatisfy` elem "MonadRandom.getRandom"
      state `shouldNotSatisfy` elem "MonadRandom.getRandomRs"

  describe "Produced UUID" $
    -- You can get valid hash (without dashes) looking at test plugs and using
    -- this bash command:
    --   perl -e 'print q/*/x128 . q/|1858-11-19 00:00:03 UTC/' | md5sum
    -- The result would be:
    --   7fa0740459a6bae355913b39ce9708fa  -

    it "Produced RequestId is correct" $
      value `shouldBe` [qn| RequestId "7fa07404-59a6-bae3-5591-3b39ce9708fa" |]

  where (TestState state (show -> value)) = newRequestId :: TestState RequestId
