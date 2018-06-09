{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Data.Function ((&))
import           Data.String (fromString)
import           Text.InterpolatedString.QM

import qualified Network.Wai.Handler.Warp as Warp

import           Carma.EraGlonass.Server (serverApplicaton)


main :: IO ()
main = do
  putStrLn [qm| Running incoming server on http://{host}:{port}... |]
  runIncomingServer port $ fromString host

  where port = 8166
        host = "127.0.0.1"


runIncomingServer :: Warp.Port -> Warp.HostPreference -> IO ()
runIncomingServer port host = Warp.runSettings warpSettings serverApplicaton
  where warpSettings
          = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setHost host
