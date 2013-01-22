{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString as BS

import Data.Dict as D
import qualified Data.Map as M

import System.Environment
import System.Exit

import Carma.HTTP
import Carma.SAGAI


exportCase ::Int -> Int -> Dict -> IO ()
exportCase caseNumber cp wazzup = do
  -- Read case with provided number and all of the associated services
  res <- readInstance cp "case" caseNumber
  servs <- forM (readReferences $ res M.! "services") 
           (\(m, i) -> do
              inst <- readInstance cp m i
              return (m, i, inst))

  fv <- runExport sagaiFullExport 0 (res, servs) cp wazzup
  case fv of
    Left err -> print err
    Right entry -> BS.putStr entry
  return ()

-- | Get a value of a field in the current service.
--servField :: FieldName -> ServEntry FieldValue


usage :: String
usage = "Usage: " ++
        "sagai-export <Wazzup.json> <CaRMa port number> <case id>"


main = do
  args <- getArgs
  when (length args /= 3) $ putStrLn usage >> exitFailure
  let (carmaPort:caseNumber:_) = map read $ drop 1 args
  Just wazzup <- loadDict (args !! 0)
  exportCase caseNumber carmaPort wazzup
