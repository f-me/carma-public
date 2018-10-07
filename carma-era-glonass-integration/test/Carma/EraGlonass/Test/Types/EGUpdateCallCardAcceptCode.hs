{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Test.Types.EGUpdateCallCardAcceptCode
     ( spec
     ) where

import           Test.Hspec

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.Text as ParsecText
import           Data.Either (isRight)
import           Data.String (IsString (fromString))

import           Control.Monad (forM_)

import           Carma.EraGlonass.Test.Helpers
import           Carma.EraGlonass.Types.EGUpdateCallCardAcceptCode


spec :: Spec
spec = do
  describe "Correctness of JSON parser" $ do
    it "All correct cases" $
      -- Producing list of all values to reduce human-factor mistakes,
      -- so if new value is added we'll get incomplete pattern-matching error.
      forM_ [minBound..maxBound] $ \x -> case x of
        OK ->
          parseEither jsonParser (String "OK") `shouldBe` Right x
        IncorrectFormat ->
          parseEither jsonParser (String "INCORRECT_FORMAT") `shouldBe` Right x
        CardNotFound ->
          parseEither jsonParser (String "CARD_NOT_FOUND") `shouldBe` Right x

    it "Fails on empty value" $ do
      let substr = findSubstring "expected EGUpdateCallCardAcceptCode"
      parseEither jsonParser (String "") `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

    it "Fails on unknown value" $ do
      let substr = findSubstring "expected EGUpdateCallCardAcceptCode"
      parseEither jsonParser (String "WUT") `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

    it "Case-sensitive" $ do
      let substr = findSubstring "expected EGUpdateCallCardAcceptCode"
          f = isRight . ParsecText.parseOnly substr . fromString
          checkError = \case Right _ -> False; Left x -> f x

      parseEither jsonParser (String "oK") `shouldSatisfy` checkError
      parseEither jsonParser (String "Ok") `shouldSatisfy` checkError
      parseEither jsonParser (String "ok") `shouldSatisfy` checkError

    it "Incorrect JSON type" $ do
      let substr = findSubstring
            "expected EGUpdateCallCardAcceptCode, encountered Number"
      parseEither jsonParser (Number 123) `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

  describe "Correctness of JSON encoder" $
    it "All correct cases" $
      -- Producing list of all values to reduce human-factor mistakes,
      -- so if new value is added we'll get incomplete pattern-matching error.
      forM_ [minBound..maxBound] $ \x -> case x of
        OK              -> toJSON x `shouldBe` String "OK"
        IncorrectFormat -> toJSON x `shouldBe` String "INCORRECT_FORMAT"
        CardNotFound    -> toJSON x `shouldBe` String "CARD_NOT_FOUND"


jsonParser :: Value -> Parser EGUpdateCallCardAcceptCode
jsonParser = parseJSON
