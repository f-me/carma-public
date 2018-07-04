{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Test.Utils.RequestId
     ( spec
     ) where

import           Test.Hspec

import           Data.Either
import           Data.Char (toLower)
import           Data.Function ((&))
import           Data.Time.Calendar
import           Data.Time.Clock hiding (getCurrentTime)
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import           Data.ByteString.Char8 (unpack)
import           Text.InterpolatedString.QM

import           Control.Monad (ap)
import           Control.Monad.Random.Class

import           System.Random

import           Carma.EraGlonass.RequestId
import           Carma.Monad.Clock
import           Carma.Utils.Operators


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
    describe "Gets current time" $ do
      it "Gets it" $
        state `shouldSatisfy` elem "MonadClock.getCurrentTime"
      it "Gets it only once" $
        state `shouldSatisfy`
          filter (== "MonadClock.getCurrentTime") ? length ? (==1)

    describe "Gets random values" $ do
      it "Gets it" $
        state `shouldSatisfy` elem "MonadRandom.getRandoms"
      it "Gets it only once" $
        state `shouldSatisfy`
          filter (== "MonadRandom.getRandoms") ? length ? (==1)

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

  describe "RequestId Parser" $ do
    it "Usual RequestId from string" $ do
      let referenceVal1 = "00000000-0000-0000-0000-000000000000"
          referenceVal2 = "7fa07404-59a6-bae3-5591-3b39ce9708fa"
          referenceVal3 = "ffffffff-ffff-ffff-ffff-ffffffffffff"

      (show <$> parseOnly requestIdParser referenceVal1) `shouldBe`
        Right [qm| RequestId "{referenceVal1}" |]
      (show <$> parseOnly requestIdParser referenceVal2) `shouldBe`
        Right [qm| RequestId "{referenceVal2}" |]
      (show <$> parseOnly requestIdParser referenceVal3) `shouldBe`
        Right [qm| RequestId "{referenceVal3}" |]

    it "Upper-case hash" $ do
      let referenceVal1 = "7FA07404-59A6-BAE3-5591-3B39CE9708FA"
          referenceVal2 = "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"

      (show <$> parseOnly requestIdParser referenceVal1) `shouldBe`
        Right [qm| RequestId "{toLower <$> unpack referenceVal1}" |]
      (show <$> parseOnly requestIdParser referenceVal2) `shouldBe`
        Right [qm| RequestId "{toLower <$> unpack referenceVal2}" |]

    it "Out of hex range is failing" $ do
      let referenceVal1 = "0000f000-0000-0000-0000-000000000000"
          referenceVal2 = "0000g000-0000-0000-0000-000000000000"
          referenceVal3 = "0000F000-0000-0000-0000-000000000000"
          referenceVal4 = "0000G000-0000-0000-0000-000000000000"

      (show <$> parseOnly requestIdParser referenceVal1) `shouldBe`
        Right [qm| RequestId "{referenceVal1}" |]
      (show <$> parseOnly requestIdParser referenceVal2) `shouldSatisfy` isLeft
      (show <$> parseOnly requestIdParser referenceVal3) `shouldBe`
        Right [qm| RequestId "{toLower <$> unpack referenceVal3}" |]
      (show <$> parseOnly requestIdParser referenceVal4) `shouldSatisfy` isLeft

    it "Misplaced dash is failing" $ do
      let referenceVal = "0000000-00000-0000-0000-000000000000"
      (show <$> parseOnly requestIdParser referenceVal) `shouldSatisfy` isLeft

    it "No dashes is failing" $ do
      let referenceVal = "00000000000000000000000000000000"
      (show <$> parseOnly requestIdParser referenceVal) `shouldSatisfy` isLeft

    it "Trailing dash is failing" $ do
      let referenceVal = "00000000-0000-0000-0000-000000000000-"
      (show <$> parseOnly requestIdParser referenceVal) `shouldSatisfy` isLeft

    it "Prefixed dash is failing" $ do
      let referenceVal = "-00000000-0000-0000-0000-000000000000"
      (show <$> parseOnly requestIdParser referenceVal) `shouldSatisfy` isLeft

    it "Not enough digits is failing" $ do
      let referenceVal1 = "00000000-0000-0000-0000-00000000000"
          referenceVal2 = "00000000-0000-0000-000-000000000000"
          referenceVal3 = "00000000-0000-000-0000-000000000000"
          referenceVal4 = "00000000-000-0000-0000-000000000000"
          referenceVal5 = "0000000-0000-0000-0000-000000000000"

      (show <$> parseOnly requestIdParser referenceVal1) `shouldSatisfy` isLeft
      (show <$> parseOnly requestIdParser referenceVal2) `shouldSatisfy` isLeft
      (show <$> parseOnly requestIdParser referenceVal3) `shouldSatisfy` isLeft
      (show <$> parseOnly requestIdParser referenceVal4) `shouldSatisfy` isLeft
      (show <$> parseOnly requestIdParser referenceVal5) `shouldSatisfy` isLeft

    it "Extra digits is failing" $ do
      let referenceVal1 = "00000000-0000-0000-0000-0000000000000"
          referenceVal2 = "00000000-0000-0000-00000-000000000000"
          referenceVal3 = "00000000-0000-00000-0000-000000000000"
          referenceVal4 = "00000000-00000-0000-0000-000000000000"
          referenceVal5 = "000000000-0000-0000-0000-000000000000"

      (show <$> parseOnly requestIdParser referenceVal1) `shouldSatisfy` isLeft
      (show <$> parseOnly requestIdParser referenceVal2) `shouldSatisfy` isLeft
      (show <$> parseOnly requestIdParser referenceVal3) `shouldSatisfy` isLeft
      (show <$> parseOnly requestIdParser referenceVal4) `shouldSatisfy` isLeft
      (show <$> parseOnly requestIdParser referenceVal5) `shouldSatisfy` isLeft

  where (TestState state (show -> value)) = newRequestId :: TestState RequestId
