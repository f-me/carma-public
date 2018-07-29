{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Test.Types.EGCreateCallCardRequest
     ( spec
     ) where

import           Test.Hspec

import           Data.Either (isRight)
import           Text.InterpolatedString.QM
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as ParsecBS
import qualified Data.Attoparsec.Text as ParsecText
import qualified Data.HashMap.Lazy as HM
import           Data.String (fromString)

import           Control.Applicative ((<|>))

import           Carma.EraGlonass.Types
import           Carma.EraGlonass.RequestId (requestIdParser)
import           Carma.Utils.Operators


spec :: Spec
spec =
  describe "Correctness of JSON parser" $ do
    let jsonParser :: Value -> Parser EGCreateCallCardRequest
        jsonParser = parseJSON

        testReferenceValue :: Either String EGCreateCallCardRequest
        testReferenceValue = do
          requestId' <-
            ParsecBS.parseOnly requestIdParser
              "c94eea91-d647-43d2-af04-109fbb53d8dc"

          pure EGCreateCallCardRequest
                 { requestId = requestId'
                 , cardIdCC = EGCallCardId "597b53edf0f012e5e00d8a9a"
                 , atPhoneNumber = EGPhoneNumber "9411000003"
                 , lastTrustedLatitude = EGLatitude 200692000
                 , lastTrustedLongitude = EGLongitude 135459000
                 , callerFullName = EGCallerFullName "Иванов Иван Иванович"
                 , callerPhoneNumber = EGPhoneNumber "79999999999"
                 , locationDescription =
                     "Описание местонахождения (заполнено операторм ФКЦ)"
                 , vehicle = EGCreateCallCardRequestVehicle
                     { vin = "1G6A85SS8H0138585"
                     , propulsion = ""
                     , color = "черный"
                     , registrationNumber = "А435УК66"
                     }
                 , gis = (:[]) EGCreateCallCardRequestGis
                     { regionName = "Москва"
                     , settlementName = "Москва"
                     , streetName = "Новолесная"
                     , building = "3"
                     }
                 }

    it "Usual test data" $ do
      let x = testData >>= parseEither jsonParser
      x `shouldBe` testReferenceValue
      isRight x `shouldBe` True
      isRight testReferenceValue `shouldBe` True

    it [qn| "locationDescription" is limited by 180 symbols |] $ do
      let maxValue = fromString $ replicate 180 'x'
          exceededValue = fromString $ replicate 181 'x'

          maxValue' = testData
            >>= (\case Object x -> Right $ Object $
                         HM.insert "locationDescription" (String maxValue) x
                       x -> Left [qm| Incorrect "locationDescription"
                                      JSON type: {x} |])
            >>= parseEither jsonParser

          exceededValue' = testData
            >>= (\case Object x -> Right $ Object $
                         HM.insert "locationDescription"
                                   (String exceededValue) x
                       x -> Left [qm| Incorrect "locationDescription"
                                      JSON type: {x} |])
            >>= parseEither jsonParser

          testReferenceValueWithMaximum = testReferenceValue
            <&> \x -> x { locationDescription = maxValue }

      maxValue' `shouldBe` testReferenceValueWithMaximum
      isRight maxValue' `shouldBe` True
      isRight testReferenceValueWithMaximum `shouldBe` True

      exceededValue' `shouldSatisfy` \case
        Right _ -> False
        Left  x -> let

          findSubstring str =
            ParsecText.try (ParsecText.string str)
              <|> (ParsecText.anyChar *> findSubstring str)

          parser = findSubstring "EGCreateCallCardRequest.locationDescription"

          in isRight $ ParsecText.parseOnly parser $ fromString x

testData :: Either String Value
testData = eitherDecodeStrict [qn|
  {
    "requestId": "c94eea91-d647-43d2-af04-109fbb53d8dc",
    "atPhoneNumber": "9411000003",
    "lastTrustedLatitude": 200692000,
    "lastTrustedLongitude": 135459000,
    "callerFullName": "\u0418\u0432\u0430\u043d\u043e\u0432 \u0418\u0432\u0430\u043d \u0418\u0432\u0430\u043d\u043e\u0432\u0438\u0447",
    "callerPhoneNumber": "79999999999",
    "locationDescription": "\u041e\u043f\u0438\u0441\u0430\u043d\u0438\u0435 \u043c\u0435\u0441\u0442\u043e\u043d\u0430\u0445\u043e\u0436\u0434\u0435\u043d\u0438\u044f (\u0437\u0430\u043f\u043e\u043b\u043d\u0435\u043d\u043e \u043e\u043f\u0435\u0440\u0430\u0442\u043e\u0440\u043c \u0424\u041a\u0426)",
    "gis": [
      {
        "regionName": "\u041c\u043e\u0441\u043a\u0432\u0430",
        "settlementName": "\u041c\u043e\u0441\u043a\u0432\u0430",
        "streetName": "\u041d\u043e\u0432\u043e\u043b\u0435\u0441\u043d\u0430\u044f",
        "building": "3"
      }
    ],
    "vehicle": {
      "vin": "1G6A85SS8H0138585",
      "propulsion": "",
      "color": "\u0447\u0435\u0440\u043d\u044b\u0439",
      "registrationNumber": "\u0410435\u0423\u041a66"
    },
    "cardIdCC": "597b53edf0f012e5e00d8a9a"
  }
|] & \case Left msg -> Left [qm| Error while parsing test data: {msg} |]
           x -> x
