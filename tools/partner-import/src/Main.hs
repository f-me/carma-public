{-|

  CLI tool for CaRMa partner import.

  Loads CSV files into partner database, producing a report.

 -}


import Control.Monad

import System.Environment
import System.Exit

import Carma.Partner

usage :: String
usage = "Usage: " ++
        "carma-partner-import 8000 dealers.csv out.csv " ++
        "DealerCities.json PSACarMakers.json TaxSchemes.json PSAServices.json"


main :: IO ()
main = do
  args <- getArgs
  when (length args /= 7) $ putStrLn usage >> exitFailure

  -- Load dictionaries
  let (cp:input:output:cityFile:carFile:taxFile:servFile:_) = args
  Just dicts <- loadIntegrationDicts cityFile carFile taxFile servFile
  let carmaPort = read cp

  processData carmaPort input output dicts
