{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Test.Types.PersistentTextKey
     ( spec
     ) where

import           Test.Hspec

import           Data.Int
import           Data.Aeson
import           Data.Aeson.Types

import           Control.Monad
import           Control.Monad.Random.Class (getRandoms)

import           Database.Persist.Sql (toSqlKey)

import           Carma.Utils.Operators
import           Carma.Model.Case.Persistent (Case)
import           Carma.EraGlonass.Types.PersistentTextKey


spec :: Spec
spec = do
  let toCaseId :: Int64 -> PersistentTextKey Case
      toCaseId = PersistentTextKey . toSqlKey

  describe "Basic instances" $ do
    it "Eq instance" $ do
      toCaseId 123   `shouldBe`    toCaseId 123
      toCaseId 123   `shouldNotBe` toCaseId 321
      toCaseId (-10) `shouldBe`    toCaseId (-10)
      toCaseId (-20) `shouldNotBe` toCaseId (-10)

    it "Show instance" $ do
      show (toCaseId 123)    `shouldBe` "123"
      show (toCaseId 0)      `shouldBe` "0"
      show (toCaseId (-1))   `shouldBe` "-1"
      show (toCaseId (-123)) `shouldBe` "-123"

  describe "Correctness of JSON parser" $ do
    let jsonParser :: Value -> Parser (PersistentTextKey Case)
        jsonParser = parseJSON

    it "Usual correct case" $ do
      fmap show (parseMaybe jsonParser $ String "120010001823039")
        `shouldBe` Just "120010001823039"
      fmap show (parseMaybe jsonParser $ String "-120010001823039")
        `shouldBe` Just "-120010001823039"

    it "Incorrect JSON type" $ do
      parseMaybe jsonParser (Number 120010001823039) `shouldBe` Nothing

    describe "Full cycle resolves to original value" $ do
      it "Static full cycle" $ do
        let testId = toCaseId 123
        (testId & toJSON & parseMaybe jsonParser) `shouldBe` Just testId

      it "Randomized full cycle" $ do
        testIds <- take 100 . fmap toCaseId <$> getRandoms
        forM_ testIds $ \testId ->
          (testId & toJSON & parseMaybe jsonParser) `shouldBe` Just testId
