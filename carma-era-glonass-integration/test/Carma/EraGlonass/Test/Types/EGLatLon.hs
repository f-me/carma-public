{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Carma.EraGlonass.Test.Types.EGLatLon
     ( spec
     ) where

import           Test.Hspec

import           Data.Either (isRight)
import           Data.Aeson
import           Data.Aeson.Types

import           Carma.EraGlonass.Types.EGLatLon ( EGLatitude,  toEGLatitude
                                                 , EGLongitude, toEGLongitude
                                                 )


spec :: Spec
spec = do
  describe "EGLatitude" $
    describe "Correctness of JSON parser" $ do
      let testParse :: Value -> Maybe EGLatitude
          testParse = parseMaybe parseJSON

      it "Usual correct value" $ do
        (testParse (Number 200692000) `shouldBe`)
          =<< extrudeRightAndDentToJust (toEGLatitude 200692000)

        (testParse (Number 0) `shouldBe`) =<<
          extrudeRightAndDentToJust (toEGLatitude 0)

      it "Incorrect JSON type" $
        testParse (String "200692000") `shouldBe` Nothing

      it "Minimum" $ do
        (testParse (Number (-648000000)) `shouldBe`)
          =<< extrudeRightAndDentToJust (toEGLatitude (-648000000))

        testParse (Number (-648000001)) `shouldBe` Nothing

      it "Maximum" $ do
        (testParse (Number 648000000) `shouldBe`)
          =<< extrudeRightAndDentToJust (toEGLatitude 648000000)

        testParse (Number 648000001) `shouldBe` Nothing

  describe "EGLongitude" $
    describe "Correctness of JSON parser" $ do
      let testParse :: Value -> Maybe EGLongitude
          testParse = parseMaybe parseJSON

      it "Usual correct value" $ do
        (testParse (Number 135459000) `shouldBe`)
          =<< extrudeRightAndDentToJust (toEGLongitude 135459000)

        (testParse (Number 0) `shouldBe`)
          =<< extrudeRightAndDentToJust (toEGLongitude 0)

      it "Incorrect JSON type" $
        testParse (String "135459000") `shouldBe` Nothing

      it "Minimum" $ do
        (testParse (Number (-324000000)) `shouldBe`)
          =<< extrudeRightAndDentToJust (toEGLongitude (-324000000))

        testParse (Number (-324000001)) `shouldBe` Nothing

      it "Maximum" $ do
        (testParse (Number 324000000) `shouldBe`)
          =<< extrudeRightAndDentToJust (toEGLongitude 324000000)

        testParse (Number 324000001) `shouldBe` Nothing


extrudeRightAndDentToJust :: (Show r, Eq r) => Either String r -> IO (Maybe r)
extrudeRightAndDentToJust x = do
  shouldSatisfy x isRight
  either (const $ fail "Unexpected behavior") (pure . Just) x
