module Main (main) where

import           Test.Hspec (hspec, describe)

import qualified Carma.EraGlonass.Test.Types.RequestId as RequestId
import qualified Carma.EraGlonass.Test.Types.EGCallCardId as EGCallCardId
import qualified Carma.EraGlonass.Test.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Test.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Test.Types.EGVin as EGVin
import qualified Carma.EraGlonass.Test.Types.EGPropulsion as EGPropulsion
import qualified Carma.EraGlonass.Test.Types.EGAcceptCode as EGAcceptCode
import qualified Carma.EraGlonass.Test.Types.EGCallerFullName
                   as EGCallerFullName
import qualified Carma.EraGlonass.Test.Types.EGCreateCallCardRequest
                   as EGCreateCallCardRequest
import qualified Carma.EraGlonass.Test.Types.EGCreateCallCardResponse
                   as EGCreateCallCardResponse
import qualified Carma.EraGlonass.Test.Types.PersistentTextKey
                   as PersistentTextKey


main :: IO ()
main = hspec $ do
  describe "RequestId" RequestId.spec
  describe "EGCallCardId" EGCallCardId.spec
  describe "EGPhoneNumber" EGPhoneNumber.spec
  describe "EGCallerFullName" EGCallerFullName.spec
  describe "EGLatitude & EGLongitude" EGLatLon.spec
  describe "EGVin" EGVin.spec
  describe "EGPropulsion" EGPropulsion.spec
  describe "EGAcceptCode" EGAcceptCode.spec
  describe "EGCreateCallCardRequest" EGCreateCallCardRequest.spec
  describe "EGCreateCallCardResponse" EGCreateCallCardResponse.spec
  describe "PersistentTextKey" PersistentTextKey.spec
