{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Test.Types.EGLatLon
     ( spec
     ) where

import           Test.Hspec

import           Data.Aeson
import           Data.Aeson.Types

import           Carma.EraGlonass.Types ( EGLatitude  (EGLatitude)
                                        , EGLongitude (EGLongitude)
                                        )


spec :: Spec
spec = do
  describe "EGLatitude" $
    describe "Correctness of JSON parser" $ do
      let testParse :: Value -> Maybe EGLatitude
          testParse = parseMaybe parseJSON

      it "Usual correct value" $ do
        testParse (Number 200692000) `shouldBe` Just (EGLatitude 200692000)
        testParse (Number 0) `shouldBe` Just (EGLatitude 0)

      it "Incorrect JSON type" $
        testParse (String "200692000") `shouldBe` Nothing

      it "Minimum" $ do
        testParse (Number (-648000000))
          `shouldBe` Just (EGLatitude (-648000000))
        testParse (Number (-648000001)) `shouldBe` Nothing

      it "Maximum" $ do
        testParse (Number 648000000) `shouldBe` Just (EGLatitude 648000000)
        testParse (Number 648000001) `shouldBe` Nothing

  describe "EGLongitude" $
    describe "Correctness of JSON parser" $ do
      let testParse :: Value -> Maybe EGLongitude
          testParse = parseMaybe parseJSON

      it "Usual correct value" $ do
        testParse (Number 135459000) `shouldBe` Just (EGLongitude 135459000)
        testParse (Number 0) `shouldBe` Just (EGLongitude 0)

      it "Incorrect JSON type" $
        testParse (String "135459000") `shouldBe` Nothing

      it "Minimum" $ do
        testParse (Number (-324000000))
          `shouldBe` Just (EGLongitude (-324000000))
        testParse (Number (-324000001)) `shouldBe` Nothing

      it "Maximum" $ do
        testParse (Number 324000000) `shouldBe` Just (EGLongitude 324000000)
        testParse (Number 324000001) `shouldBe` Nothing
