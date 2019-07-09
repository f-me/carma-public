{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Test.Types.EGVin
     ( spec
     ) where

import           Test.Hspec

import           Data.Aeson
import           Data.Aeson.Types

import           Carma.EraGlonass.Types.EGVin.Internal (EGVin (EGVin))


spec :: Spec
spec =
  describe "Correctness of JSON parser" $ do
    let jsonParser :: Value -> Parser EGVin
        jsonParser = parseJSON

    it "Usual VIN value" $
      parseMaybe jsonParser (String "1G6A85SS8H0138585") `shouldBe`
        Just (EGVin "1G6A85SS8H0138585")

    it "Incorrect JSON type" $ do
      -- Checking if the same value with string is correct
      parseMaybe jsonParser (String "11111111111111111") `shouldBe`
        Just (EGVin "11111111111111111")

      parseMaybe jsonParser (Number 11111111111111111) `shouldBe` Nothing

    it "Empty value will fail" $
      parseMaybe jsonParser (String "") `shouldBe` Nothing

    it "Exactly 17 characters" $ do
      parseMaybe jsonParser (String "1G6A85SS8H013858") `shouldBe` Nothing
      parseMaybe jsonParser (String "1G6A85SS8H01385856") `shouldBe` Nothing

      parseMaybe jsonParser (String "1G6A85SS8H0138585") `shouldBe`
        Just (EGVin "1G6A85SS8H0138585")

      parseMaybe jsonParser (String "1111111111111111") `shouldBe` Nothing
      parseMaybe jsonParser (String "111111111111111111") `shouldBe` Nothing

      parseMaybe jsonParser (String "11111111111111111") `shouldBe`
        Just (EGVin "11111111111111111")

    it "IOQ are prohibited symbols" $ do
      parseMaybe jsonParser (String "1G6A85SS8HI138585") `shouldBe` Nothing
      parseMaybe jsonParser (String "1G6A85SS8HO138585") `shouldBe` Nothing
      parseMaybe jsonParser (String "1G6A85SS8HQ138585") `shouldBe` Nothing

    it "Keeps result case-sensitive" $
      parseMaybe jsonParser (String "1g6a85SS8h0138585") `shouldBe`
        Just (EGVin "1g6a85SS8h0138585")
