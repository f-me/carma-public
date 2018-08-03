module Main (main) where

import           Test.Hspec (hspec, describe)

import qualified Carma.EraGlonass.Test.Types.EGCallCardId as EGCallCardId
import qualified Carma.EraGlonass.Test.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Test.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Test.Types.EGVin as EGVin
import qualified Carma.EraGlonass.Test.Types.EGPropulsion as EGPropulsion
import qualified Carma.EraGlonass.Test.Types.EGCallerFullName
                   as EGCallerFullName
import qualified Carma.EraGlonass.Test.Types.EGCreateCallCardRequest
                   as EGCreateCallCardRequest


main :: IO ()
main = hspec $ do
  describe "EGCallCardId" EGCallCardId.spec
  describe "EGPhoneNumber" EGPhoneNumber.spec
  describe "EGCallerFullName" EGCallerFullName.spec
  describe "EGLatitude & EGLongitude" EGLatLon.spec
  describe "EGVin" EGVin.spec
  describe "EGPropulsion" EGPropulsion.spec
  describe "EGCreateCallCardRequest" EGCreateCallCardRequest.spec
