module Main (main) where

import           Test.Hspec (hspec, describe)

import qualified Carma.EraGlonass.Test.Types.EGPhoneNumber as EGPhoneNumber


main :: IO ()
main = hspec $
  describe "EGPhoneNumber" EGPhoneNumber.spec
