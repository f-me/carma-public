{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Test.Types.EGPhoneNumber
     ( spec
     ) where

import           Test.Hspec

import           Data.Aeson
import           Data.Aeson.Types

import           Carma.EraGlonass.Types.EGPhoneNumber
                   (EGPhoneNumber (EGPhoneNumber))


spec :: Spec
spec =
  describe "Correctness of JSON parser" $ do
    let jsonParser :: Value -> Parser EGPhoneNumber
        jsonParser = parseJSON

    it "Usual phone number" $ do
      parseMaybe jsonParser (String "+79005001020") `shouldBe`
        Just (EGPhoneNumber "+79005001020")

      -- no plus
      parseMaybe jsonParser (String "79005001020") `shouldBe`
        Just (EGPhoneNumber "79005001020")

    it "Incorrect JSON type" $
      parseMaybe jsonParser (Number 79005001020) `shouldBe` Nothing

    it "Minimum correct" $ do
      parseMaybe jsonParser (String "0") `shouldBe` Just (EGPhoneNumber "0")
      parseMaybe jsonParser (String "1") `shouldBe` Just (EGPhoneNumber "1")
      parseMaybe jsonParser (String "9") `shouldBe` Just (EGPhoneNumber "9")

    it "Just plus is failing" $
      parseMaybe jsonParser (String "+") `shouldBe` Nothing

    it "Empty string is failing" $
      parseMaybe jsonParser (String "") `shouldBe` Nothing

    it "Maximum" $ do
      parseMaybe jsonParser (String "00000000000000000") `shouldBe`
        Just (EGPhoneNumber "00000000000000000")
      parseMaybe jsonParser (String "12345678901234567") `shouldBe`
        Just (EGPhoneNumber "12345678901234567")
      parseMaybe jsonParser (String "99999999999999999") `shouldBe`
        Just (EGPhoneNumber "99999999999999999")

    it "Adding plus prefix does not exceed maximum" $ do
      parseMaybe jsonParser (String "+00000000000000000") `shouldBe`
        Just (EGPhoneNumber "+00000000000000000")
      parseMaybe jsonParser (String "+12345678901234567") `shouldBe`
        Just (EGPhoneNumber "+12345678901234567")
      parseMaybe jsonParser (String "+99999999999999999") `shouldBe`
        Just (EGPhoneNumber "+99999999999999999")

    it "One more than maximum is failing" $ do
      parseMaybe jsonParser (String "000000000000000000") `shouldBe` Nothing
      parseMaybe jsonParser (String "123456789012345678") `shouldBe` Nothing
      parseMaybe jsonParser (String "999999999999999999") `shouldBe` Nothing
      parseMaybe jsonParser (String "+000000000000000000") `shouldBe` Nothing
      parseMaybe jsonParser (String "+123456789012345678") `shouldBe` Nothing
      parseMaybe jsonParser (String "+999999999999999999") `shouldBe` Nothing

    it "Doubled plus is failing" $ do
      parseMaybe jsonParser (String "++79005001020") `shouldBe` Nothing
      parseMaybe jsonParser (String "++7") `shouldBe` Nothing

    it "Misplaced plus is failing" $ do
      parseMaybe jsonParser (String "7+9005001020") `shouldBe` Nothing
      parseMaybe jsonParser (String "79005001020+") `shouldBe` Nothing
      parseMaybe jsonParser (String "7+") `shouldBe` Nothing

    it "Not a number is failing" $ do
      parseMaybe jsonParser (String "7900a001020") `shouldBe` Nothing
      parseMaybe jsonParser (String "7a005001020") `shouldBe` Nothing

    it "Not a number suffix is failing" $ do
      parseMaybe jsonParser (String "7900500102x") `shouldBe` Nothing
      parseMaybe jsonParser (String "79005001020x") `shouldBe` Nothing

    it "Minus suffix is failing (could be negative number)" $ do
      parseMaybe jsonParser (String "-79005001020") `shouldBe` Nothing
