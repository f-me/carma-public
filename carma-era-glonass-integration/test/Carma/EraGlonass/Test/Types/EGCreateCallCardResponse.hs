{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Carma.EraGlonass.Test.Types.EGCreateCallCardResponse
     ( spec
     ) where

import           Test.Hspec

import           Data.Monoid ((<>))
import           Data.Aeson
import qualified Data.HashMap.Lazy as HM
import           Text.InterpolatedString.QM

import           Database.Persist.Sql (toSqlKey)

import           Carma.EraGlonass.Types.PersistentTextKey
import           Carma.EraGlonass.Types.EGCreateCallCardRequest
import           Carma.EraGlonass.Types.EGCreateCallCardAcceptCode


spec :: Spec
spec =
  describe "Correctness of JSON encoder" $ do
    let (!jsonRef, !rootJsonRefObjMap) =
          case jsonReference of
               Right y@(Object z) -> (y, z)
               Right x -> error [qms| Incorrect JSON type of root object of
                                      JSON test reference, value: {x} |]
               Left  e -> error [qms| JSON test reference is incorrect,
                                      error: {e} |]

    it "Correct test example" $ do
      encode testReference `shouldBe` encode jsonRef

    it "Failure case" $ do
      let brokenTestRefA = testReference { acceptCode = InternalError }
          brokenTestRefB = testReference { statusDescription = Just "foo" }

      -- Broken "EGCreateCallCardResponse"
      encode brokenTestRefA `shouldNotBe` encode jsonRef
      encode brokenTestRefB `shouldNotBe` encode jsonRef

      let brokenJSONRefA = Object $ HM.delete "responseId" rootJsonRefObjMap
          brokenJSONRefB = Object $
            let
              f = \case String x -> Just $ String $ x <> "123"
                        x -> error [qm| Unexpected JSON value: {x} |]
            in
              HM.update f "cardidProvider" rootJsonRefObjMap

      -- Correctness of reconstructed "Object"
      encode testReference `shouldBe` encode (Object rootJsonRefObjMap)
      -- Broken JSON test example
      encode testReference `shouldNotBe` encode brokenJSONRefA
      encode testReference `shouldNotBe` encode brokenJSONRefB

    it [qn| "statusDescription" is optional (key could be not set) |] $ do
      let modifiedTestRef = testReference { statusDescription = Nothing }
          modifiedJsonRef =
            Object $ HM.delete "statusDescription" rootJsonRefObjMap

      encode modifiedTestRef `shouldBe` encode modifiedJsonRef


testReference :: EGCreateCallCardResponse
testReference
  = EGCreateCallCardResponse
  { responseId        = "177551"
  , cardidProvider    = PersistentTextKey $ toSqlKey 120010001823039
  , acceptId          = "597b53edf0f012e5e00d8a9a"
  , requestId         = "9db7cf43-deab-4c27-a8df-74bec0b75df1"
  , acceptCode        = OK
  , statusDescription = Just ""
  }


jsonReference :: Either String Value
jsonReference = eitherDecodeStrict [qn|
  {
    "responseId": "177551",
    "cardidProvider": "120010001823039",
    "acceptId": "597b53edf0f012e5e00d8a9a",
    "requestId": "9db7cf43-deab-4c27-a8df-74bec0b75df1",
    "acceptCode": "OK",
    "statusDescription": ""
  }
|]
