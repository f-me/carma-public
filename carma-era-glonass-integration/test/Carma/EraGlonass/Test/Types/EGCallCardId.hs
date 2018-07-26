{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Carma.EraGlonass.Test.Types.EGCallCardId
     ( spec
     ) where


import           Test.Hspec

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text

import           Control.Monad (forM, forM_)
import           Control.Monad.Random.Class (getRandoms)

import           Carma.EraGlonass.Types.EGCallCardId
                   (EGCallCardId (EGCallCardId))


spec :: Spec
spec =
  describe "Correctness of JSON parser" $ do
    let testParse :: Value -> Maybe EGCallCardId
        testParse = parseMaybe parseJSON

    it "Usual correct value" $ do
      testParse (String "123") `shouldBe` Just (EGCallCardId "123")
      testParse (String "597b53edf0f012e5e00d8a9a")
        `shouldBe` Just (EGCallCardId "597b53edf0f012e5e00d8a9a")

    it "Incorrect JSON type" $
      testParse (Number 123) `shouldBe` Nothing

    it "At least one char is required (non-empty string)" $ do
      testParse (String "0") `shouldBe` Just (EGCallCardId "0")
      testParse (String "1") `shouldBe` Just (EGCallCardId "1")
      testParse (String "9") `shouldBe` Just (EGCallCardId "9")
      testParse (String "a") `shouldBe` Just (EGCallCardId "a")
      testParse (String "f") `shouldBe` Just (EGCallCardId "f")
      testParse (String "") `shouldBe` Nothing

    it "It is possible to have case-sensitive value" $ do
      testParse (String "ffAAbbCC") `shouldBe` Just (EGCallCardId "ffAAbbCC")
      testParse (String "1a2B3c4D") `shouldBe` Just (EGCallCardId "1a2B3c4D")

    it "It is a free string" $ do
      (randomStrings :: [Data.Text.Text]) <-
        forM ([1..10] :: [Int]) $ const $
          Data.Text.pack . take 100 <$> getRandoms

      forM_ randomStrings $ \x ->
        testParse (String x) `shouldBe` Just (EGCallCardId x)
