module Main (main) where

import           Test.Hspec (hspec, describe)

import qualified Carma.EraGlonass.Test.Types.EGRequestId as EGRequestId
import qualified Carma.EraGlonass.Test.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Test.Types.EGVin as EGVin


main :: IO ()
main = hspec $ do
  describe "EGRequestId" EGRequestId.spec
  describe "EGLatitude & EGLongitude" EGLatLon.spec
  describe "EGVin" EGVin.spec
