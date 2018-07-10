{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Test.Types.EGCallerFullName
     ( spec
     ) where


import           Test.Hspec

import qualified Data.Text
import           Data.Aeson
import           Data.Aeson.Types

import           Carma.EraGlonass.Types.EGCallerFullName
                   (EGCallerFullName (EGCallerFullName))


spec :: Spec
spec =
  describe "Correctness of JSON parser" $ do
    let testParse :: Value -> Maybe EGCallerFullName
        testParse = parseMaybe parseJSON

    it "Usual correct value" $ do
      testParse (String "Foo Bar Baz")
        `shouldBe` Just (EGCallerFullName "Foo Bar Baz")
      testParse (String "Иванов Иван Иванович")
        `shouldBe` Just (EGCallerFullName "Иванов Иван Иванович")
      testParse (String "123")
        `shouldBe` Just (EGCallerFullName "123")

    it "Incorrect JSON type" $
      testParse (Number 123) `shouldBe` Nothing

    it "Empty string is allowed" $
      testParse (String "") `shouldBe` Just (EGCallerFullName "")

    it "Exceeded maximum is failing" $ do
      let stringIsFull    = Data.Text.replicate 50 "x"
          exceededMaximum = Data.Text.replicate 51 "x"

      testParse (String stringIsFull) `shouldBe`
        Just (EGCallerFullName stringIsFull)

      testParse (String exceededMaximum) `shouldBe` Nothing
