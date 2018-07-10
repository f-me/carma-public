module Main (main) where

import           Test.Hspec (hspec, describe)

import qualified Carma.EraGlonass.Test.Types.EGCallCardId as EGCallCardId
import qualified Carma.EraGlonass.Test.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Test.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Test.Types.EGCallerFullName
                   as EGCallerFullName


main :: IO ()
main = hspec $ do
  describe "EGCallCardId" EGCallCardId.spec
  describe "EGPhoneNumber" EGPhoneNumber.spec
  describe "EGCallerFullName" EGCallerFullName.spec
  describe "EGLatitude & EGLongitude" EGLatLon.spec
