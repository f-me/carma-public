{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Text.InterpolatedString.QM

import           System.IO (stderr, hPutStrLn)
import           System.Exit (ExitCode (ExitFailure), exitWith)


main :: IO ()
main = do
  hPutStrLn stderr
    [qms| TODO not implemented yet!
          Simulating creating Call Card by Era Glonass
          ("carma-era-glonass-integration" server must be run)... |]

  exitWith $ ExitFailure 1
