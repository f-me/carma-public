{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Test.Types.EGAcceptCode
     ( spec
     ) where

import           Test.Hspec

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.Text as ParsecText
import           Data.Either (isRight)
import           Data.String (IsString (fromString))

import           Carma.EraGlonass.Test.Helpers
import           Carma.EraGlonass.Types.EGAcceptCode


spec :: Spec
spec = do
  describe "Correctness of JSON parser" $ do
    it "All correct cases" $
      parseEither jsonParser (String "OK") `shouldBe` Right OK

    it "Fails on empty value" $ do
      let substr = findSubstring "expected EGAcceptCode"
      parseEither jsonParser (String "") `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

    it "Fails on unknown value" $ do
      let substr = findSubstring "expected EGAcceptCode"
      parseEither jsonParser (String "WUT") `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

    it "Case-sensitive" $ do
      let substr = findSubstring "expected EGAcceptCode"
          f = isRight . ParsecText.parseOnly substr . fromString
          checkError = \case Right _ -> False; Left x -> f x

      parseEither jsonParser (String "oK") `shouldSatisfy` checkError
      parseEither jsonParser (String "Ok") `shouldSatisfy` checkError
      parseEither jsonParser (String "ok") `shouldSatisfy` checkError

    it "Incorrect JSON type" $ do
      let substr = findSubstring "expected EGAcceptCode, encountered Number"
      parseEither jsonParser (Number 123) `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

  describe "Correctness of JSON encoder" $
    it "Usual case" $
      toJSON OK `shouldBe` String "OK"


jsonParser :: Value -> Parser EGAcceptCode
jsonParser = parseJSON
