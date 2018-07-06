module Main (main) where

import           Test.Hspec (hspec, describe)

import qualified Carma.EraGlonass.Test.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Test.Types.EGLatLon as EGLatLon


main :: IO ()
main = hspec $ do
  describe "EGPhoneNumber" EGPhoneNumber.spec
  describe "EGLatitude & EGLongitude" EGLatLon.spec
