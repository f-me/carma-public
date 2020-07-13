{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Test.Types.EGRequestId
     ( spec
     ) where

import           Test.Hspec

import           Data.Either
import           Data.Char (toLower)
import           Data.Time.Calendar
import           Data.Time.Clock hiding (getCurrentTime)
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import           Data.ByteString.Char8 (pack, unpack)
import           Text.InterpolatedString.QM

import           Control.Monad (ap)
import           Control.Monad.Random.Class

import           System.Random

import           Carma.EraGlonass.Types.EGRequestId
import           Carma.Monad.Clock
import           Carma.Utils.Operators


-- | Value contatiner with action mark accumulator.
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
  next  TestRandomGen = (minBound + 42, TestRandomGen) -- for "Char" it's '*'
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
  let (TestState state (show -> value)) =
        newEGRequestId :: TestState EGRequestId

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

    it "Does not touch other random functions" $ do
      state `shouldNotSatisfy` elem "MonadRandom.getRandomR"
      state `shouldNotSatisfy` elem "MonadRandom.getRandom"
      state `shouldNotSatisfy` elem "MonadRandom.getRandomRs"

  describe "Produced UUID" $
    -- You can get valid hash (without dashes) looking at test plugs and using
    -- this bash command:
    --   perl -e 'print q/*/x128 . q/|1858-11-19 00:00:03 UTC/' | md5sum
    -- The result would be:
    --   7fa0740459a6bae355913b39ce9708fa  -

    it "Produced EGRequestId is correct" $
      value `shouldBe` [qn|EGRequestId "7fa07404-59a6-bae3-5591-3b39ce9708fa"|]

  describe "EGRequestId Parser" $ do
    it "Usual EGRequestId from string" $ do
      let referenceVal1 = "00000000-0000-0000-0000-000000000000"
          referenceVal2 = "7fa07404-59a6-bae3-5591-3b39ce9708fa"
          referenceVal3 = "ffffffff-ffff-ffff-ffff-ffffffffffff"

      (show <$> parseOnly egRequestIdParser referenceVal1) `shouldBe`
        Right [qm| EGRequestId "{referenceVal1}" |]
      (show <$> parseOnly egRequestIdParser referenceVal2) `shouldBe`
        Right [qm| EGRequestId "{referenceVal2}" |]
      (show <$> parseOnly egRequestIdParser referenceVal3) `shouldBe`
        Right [qm| EGRequestId "{referenceVal3}" |]

    it "Upper-case hash" $ do
      let referenceVal1 = "7FA07404-59A6-BAE3-5591-3B39CE9708FA"
          referenceVal2 = "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"

      (show <$> parseOnly egRequestIdParser referenceVal1) `shouldBe`
        Right [qm| EGRequestId "{toLower <$> unpack referenceVal1}" |]
      (show <$> parseOnly egRequestIdParser referenceVal2) `shouldBe`
        Right [qm| EGRequestId "{toLower <$> unpack referenceVal2}" |]

    it "Out of hex range is failing" $ do
      let referenceVal1 = "0000f000-0000-0000-0000-000000000000"
          referenceVal2 = "0000g000-0000-0000-0000-000000000000"
          referenceVal3 = "0000F000-0000-0000-0000-000000000000"
          referenceVal4 = "0000G000-0000-0000-0000-000000000000"

      (show <$> parseOnly egRequestIdParser referenceVal1) `shouldBe`
        Right [qm| EGRequestId "{referenceVal1}" |]
      (show <$> parseOnly egRequestIdParser referenceVal2) `shouldSatisfy`
        isLeft
      (show <$> parseOnly egRequestIdParser referenceVal3) `shouldBe`
        Right [qm| EGRequestId "{toLower <$> unpack referenceVal3}" |]
      (show <$> parseOnly egRequestIdParser referenceVal4) `shouldSatisfy`
        isLeft

    it "Misplaced dash is failing" $ do
      let referenceVal = "0000000-00000-0000-0000-000000000000"
      (show <$> parseOnly egRequestIdParser referenceVal) `shouldSatisfy`
        isLeft

    it "No dashes is failing" $ do
      let referenceVal = "00000000000000000000000000000000"
      (show <$> parseOnly egRequestIdParser referenceVal) `shouldSatisfy`
        isLeft

    it "Trailing dash is failing" $ do
      let referenceVal = "00000000-0000-0000-0000-000000000000-"
      (show <$> parseOnly egRequestIdParser referenceVal) `shouldSatisfy`
        isLeft

    it "Prefixed dash is failing" $ do
      let referenceVal = "-00000000-0000-0000-0000-000000000000"
      (show <$> parseOnly egRequestIdParser referenceVal) `shouldSatisfy`
        isLeft

    it "Not enough digits is failing" $ do
      let referenceVal1 = "00000000-0000-0000-0000-00000000000"
          referenceVal2 = "00000000-0000-0000-000-000000000000"
          referenceVal3 = "00000000-0000-000-0000-000000000000"
          referenceVal4 = "00000000-000-0000-0000-000000000000"
          referenceVal5 = "0000000-0000-0000-0000-000000000000"

      (show <$> parseOnly egRequestIdParser referenceVal1) `shouldSatisfy`
        isLeft
      (show <$> parseOnly egRequestIdParser referenceVal2) `shouldSatisfy`
        isLeft
      (show <$> parseOnly egRequestIdParser referenceVal3) `shouldSatisfy`
        isLeft
      (show <$> parseOnly egRequestIdParser referenceVal4) `shouldSatisfy`
        isLeft
      (show <$> parseOnly egRequestIdParser referenceVal5) `shouldSatisfy`
        isLeft

    it "Extra digits is failing" $ do
      let referenceVal1 = "00000000-0000-0000-0000-0000000000000"
          referenceVal2 = "00000000-0000-0000-00000-000000000000"
          referenceVal3 = "00000000-0000-00000-0000-000000000000"
          referenceVal4 = "00000000-00000-0000-0000-000000000000"
          referenceVal5 = "000000000-0000-0000-0000-000000000000"

      (show <$> parseOnly egRequestIdParser referenceVal1) `shouldSatisfy`
        isLeft
      (show <$> parseOnly egRequestIdParser referenceVal2) `shouldSatisfy`
        isLeft
      (show <$> parseOnly egRequestIdParser referenceVal3) `shouldSatisfy`
        isLeft
      (show <$> parseOnly egRequestIdParser referenceVal4) `shouldSatisfy`
        isLeft
      (show <$> parseOnly egRequestIdParser referenceVal5) `shouldSatisfy`
        isLeft

    it [qns| Getting new, converting to string and parsing it back to
             EGRequestId (full cycle) |] $ do

      let referenceVal = "7fa07404-59a6-bae3-5591-3b39ce9708fa"

      let ('E':'G':'R':'e':'q':'u':'e':'s':'t':'I':'d':' ':'"':
            (init -> pack -> x)) = value

      x `shouldBe` referenceVal

      (show <$> parseOnly egRequestIdParser x) `shouldBe`
        Right [qm| EGRequestId "{referenceVal}" |]
