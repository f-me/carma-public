{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Test.Types.EGPropulsion
     ( spec
     ) where


import           Test.Hspec

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.Text as ParsecText
import           Data.String (IsString (fromString))
import           Data.Either (isRight)
import           Text.InterpolatedString.QM

import           Carma.EraGlonass.Test.Helpers
import           Carma.EraGlonass.Types.EGPropulsion


spec :: Spec
spec =
  describe "Correctness of JSON parser" $ do
    it "All correct cases" $ do
      parseEither jsonParser (String "HYDROGEN")    `shouldBe` Right Hydrogen
      parseEither jsonParser (String "ELECTRICITY") `shouldBe` Right Electricity
      parseEither jsonParser (String "LPG")         `shouldBe` Right LPG
      parseEither jsonParser (String "LNG")         `shouldBe` Right LNG
      parseEither jsonParser (String "DIESEL")      `shouldBe` Right Diesel
      parseEither jsonParser (String "GASOLINE")    `shouldBe` Right Gasoline

    it "Incorrect JSON type" $ do
      let substr = findSubstring "expected EGPropulsion, encountered Number"
      parseEither jsonParser (Number 123) `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

    it [qns| Empty string will fail
             (except in context of "EGCreateCallCardRequestVehicle") |] $
      parseMaybe jsonParser (String "") `shouldBe` Nothing

    it "Case-sensitive" $ do
      parseMaybe jsonParser (String "HyDROGEN")    `shouldBe` Nothing
      parseMaybe jsonParser (String "HYDRoGEN")    `shouldBe` Nothing
      parseMaybe jsonParser (String "electricity") `shouldBe` Nothing
      parseMaybe jsonParser (String "LpG")         `shouldBe` Nothing
      parseMaybe jsonParser (String "LNg")         `shouldBe` Nothing
      parseMaybe jsonParser (String "Diesel")      `shouldBe` Nothing
      parseMaybe jsonParser (String "gASOLINE")    `shouldBe` Nothing

    it "Unknown value will fail" $
      parseMaybe jsonParser (String "SMTH") `shouldBe` Nothing


jsonParser :: Value -> Parser EGPropulsion
jsonParser = parseJSON
