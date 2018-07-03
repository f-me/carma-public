module Main (main) where

import           Test.Hspec (hspec, describe)

import qualified Carma.EraGlonass.Test.Utils.RequestId as RequestId


main :: IO ()
main = hspec $
  describe "RequestId" RequestId.spec
