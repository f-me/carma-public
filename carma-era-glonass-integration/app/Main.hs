{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Function ((&))
import qualified Data.Configurator as Conf
import           Data.String (fromString)
import           Text.InterpolatedString.QM

import qualified Network.Wai.Handler.Warp as Warp

import           Carma.EraGlonass.Server (serverApplicaton)


main :: IO ()
main = do
  cfg <- Conf.load [Conf.Required "app.cfg"]

  !(port :: Warp.Port) <- Conf.require cfg "port"
  !(host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"

  putStrLn [qm| Running incoming server on http://{host}:{port}... |]
  runIncomingServer port $ fromString host


runIncomingServer :: Warp.Port -> Warp.HostPreference -> IO ()
runIncomingServer port host = Warp.runSettings warpSettings serverApplicaton
  where warpSettings
          = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setHost host
