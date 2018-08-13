{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Test.Types.EGCreateCallCardRequest
     ( spec
     , testData
     ) where

import           Test.Hspec

import           Data.Monoid ((<>))
import           Data.Either (isRight)
import           Data.Either.Combinators (mapLeft)
import           Text.InterpolatedString.QM
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as ParsecBS
import qualified Data.Attoparsec.Text as ParsecText
import qualified Data.HashMap.Lazy as HM
import           Data.String (fromString)
import           Data.Text (Text)

import           Control.Monad (forM_)
import           Control.Monad.Random.Class

import           Carma.EraGlonass.Test.Helpers
import           Carma.EraGlonass.Types.EGCreateCallCardRequest
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon
import           Carma.EraGlonass.Types.RequestId (requestIdParser)
import           Carma.Utils.Operators


spec :: Spec
spec =
  describe "Correctness of JSON parser" $ do
    it "Usual test data" $ do
      let x = testData >>= parseEither jsonParser
      x `shouldBe` testReference
      isRight x `shouldBe` True
      isRight testReference `shouldBe` True

    describe [qn| "locationDescription" key |] $
      locationDescriptionJSONParserSpec

    describe [qns| Sub-type "EGCreateCallCardRequestVehicle"
                   ("vehicle" branch of JSON object) |] $

      describe [qn| "color" key |] $
        vehicleColorJSONSpec


locationDescriptionJSONParserSpec :: Spec
locationDescriptionJSONParserSpec = do
  let objLens x obj = obj { locationDescription = x }

  let overrideObjKey newValue = \case
        Object x -> Right $ Object $ HM.insert objKey newValue x
        x -> Left [qms| Incorrect JSON type of root of test data: {x} |]

  it [qn| Empty value is allowed |] $ do
    let reference = testReference <&> objLens ""

    let parsed =
          testData >>= overrideObjKey (String "") >>= parseEither jsonParser

    parsed `shouldBe` reference
    isRight parsed `shouldBe` True
    isRight reference `shouldBe` True

    -- Making sure we really changed something
    parsed `shouldNotBe` testReference

  it [qms| Limited by {limitedBy} symbols |] $ do

    let f maxValue exceededValue = -- Producing data for testing
          (maximumReference, parsedMax, parsedExceeded)
          where
            maximumReference = testReference <&> objLens maxValue

            parsedMax = testData
              >>= overrideObjKey (String maxValue)
              >>= parseEither jsonParser

            parsedExceeded = testData
              >>= overrideObjKey (String exceededValue)
              >>= parseEither jsonParser

    do -- Determinted testing
      let (maximumReference, parsedMax, parsedExceeded) =
            f (fromString $ replicate limitedBy        'x')
              (fromString $ replicate (succ limitedBy) 'x')

      parsedMax `shouldBe` maximumReference
      isRight parsedMax `shouldBe` True
      isRight maximumReference `shouldBe` True

      parsedExceeded `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

      -- Making sure we really changed something
      parsedMax `shouldNotBe` testReference

    forM_ ([1..10] :: [Int]) $ const $ do -- Randomized ten times testing
      (maximumReference, parsedMax, parsedExceeded) <-
        f <$> (fromString . take limitedBy        <$> getRandoms)
          <*> (fromString . take (succ limitedBy) <$> getRandoms)

      parsedMax `shouldBe` maximumReference
      isRight parsedMax `shouldBe` True
      isRight maximumReference `shouldBe` True

      parsedExceeded `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

      -- Making sure we really changed something
      parsedMax `shouldNotBe` testReference

  where limitedBy = 180
        objKey    = "locationDescription" :: Text
        substr    = findSubstring [qm| EGCreateCallCardRequest.{objKey} |]


vehicleColorJSONSpec :: Spec
vehicleColorJSONSpec = do
  let objLens x obj = obj { vehicle = (vehicle obj) { color = x } }

  let overrideObjKey newValue = \case
        Object rootObj -> case HM.lookup "vehicle" rootObj of
          Just (Object vehicleObj) ->
            Right $ Object $ flip (HM.insert "vehicle") rootObj
                  $ Object $ HM.insert objKey newValue vehicleObj
          Nothing ->
            Left [qm| "vehicle" key not found in test data |]
          Just x ->
            Left [qm| "vehicle" key has incorrect JSON type: {x} |]

        x ->
          Left [qms| Incorrect JSON type of root of test data: {x} |]

  it [qn| Empty value is allowed |] $ do
    let reference = testReference <&> objLens ""

    let parsed =
          testData >>= overrideObjKey (String "") >>= parseEither jsonParser

    parsed `shouldBe` reference
    isRight parsed `shouldBe` True
    isRight reference `shouldBe` True

    -- Making sure we really changed something
    parsed `shouldNotBe` testReference

  it [qm| Limited by {limitedBy} symbols |] $ do

    let f maxValue exceededValue = -- Producing data for testing
          (maximumReference, parsedMax, parsedExceeded)
          where
            maximumReference = testReference <&> objLens maxValue

            parsedMax = testData
              >>= overrideObjKey (String maxValue)
              >>= parseEither jsonParser

            parsedExceeded = testData
              >>= overrideObjKey (String exceededValue)
              >>= parseEither jsonParser

    do -- Determinted testing
      let (maximumReference, parsedMax, parsedExceeded) =
            f (fromString $ replicate limitedBy        'x')
              (fromString $ replicate (succ limitedBy) 'x')

      parsedMax `shouldBe` maximumReference
      isRight parsedMax `shouldBe` True
      isRight maximumReference `shouldBe` True

      parsedExceeded `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

      -- Making sure we really changed something
      parsedMax `shouldNotBe` testReference

    forM_ ([1..10] :: [Int]) $ const $ do -- Randomized ten times testing
      (maximumReference, parsedMax, parsedExceeded) <-
        f <$> (fromString . take limitedBy        <$> getRandoms)
          <*> (fromString . take (succ limitedBy) <$> getRandoms)

      parsedMax `shouldBe` maximumReference
      isRight parsedMax `shouldBe` True
      isRight maximumReference `shouldBe` True

      parsedExceeded `shouldSatisfy` \case
        Right _ -> False
        Left  x -> isRight $ ParsecText.parseOnly substr $ fromString x

      -- Making sure we really changed something
      parsedMax `shouldNotBe` testReference

  where limitedBy = 50
        objKey    = "color" :: Text
        substr    = findSubstring [qm|EGCreateCallCardRequestVehicle.{objKey}|]


jsonParser :: Value -> Parser EGCreateCallCardRequest
jsonParser = parseJSON


testReference :: Either String EGCreateCallCardRequest
testReference = do
  requestId' <-
    ParsecBS.parseOnly requestIdParser
      "c94eea91-d647-43d2-af04-109fbb53d8dc"

  pure EGCreateCallCardRequest
         { requestId = requestId'
         , cardIdCC = "597b53edf0f012e5e00d8a9a"
         , atPhoneNumber = "9411000003"
         , lastTrustedLatitude = EGLatLon.toEGLatitude 200692000
         , lastTrustedLongitude = EGLatLon.toEGLongitude 135459000
         , callerFullName = "Иванов Иван Иванович"
         , callerPhoneNumber = "79999999999"
         , locationDescription =
             "Описание местонахождения (заполнено операторм ФКЦ)"
         , vehicle = EGCreateCallCardRequestVehicle
             { vin = "1G6A85SS8H0138585"
             , propulsion = Nothing
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


testData :: Either String Value
testData = [qn|
  {
    "requestId": "c94eea91-d647-43d2-af04-109fbb53d8dc",
    "atPhoneNumber": "9411000003",
    "lastTrustedLatitude": 200692000,
    "lastTrustedLongitude": 135459000,
    "callerFullName":
      "\u0418\u0432\u0430\u043d\u043e\u0432
      \ \u0418\u0432\u0430\u043d
      \ \u0418\u0432\u0430\u043d\u043e\u0432\u0438\u0447",
    "callerPhoneNumber": "79999999999",
    "locationDescription":
      "\u041e\u043f\u0438\u0441\u0430\u043d\u0438\u0435
      \ \u043c\u0435\u0441\u0442\u043e\u043d\u0430\u0445\u043e\u0436\u0434\u0435
      \u043d\u0438\u044f
      \ (\u0437\u0430\u043f\u043e\u043b\u043d\u0435\u043d\u043e
      \ \u043e\u043f\u0435\u0440\u0430\u0442\u043e\u0440\u043c
      \ \u0424\u041a\u0426)",
    "gis": [
      {
        "regionName": "\u041c\u043e\u0441\u043a\u0432\u0430",
        "settlementName": "\u041c\u043e\u0441\u043a\u0432\u0430",
        "streetName":
          "\u041d\u043e\u0432\u043e\u043b\u0435\u0441\u043d\u0430\u044f",
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
|] & eitherDecodeStrict
   & mapLeft ("Error while parsing test data: " <>)
