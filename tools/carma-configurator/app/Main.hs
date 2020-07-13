{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Data.Proxy
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Text.InterpolatedString.QM
import           Data.String (IsString)

import           System.Environment (getArgs)

import           Carma.Utils.Operators
import           Carma.Configurator.Model.ToolsConfig


main :: IO ()
main = do
  (isTesting, !configType, etcArgs) <- let
    cfgTypeIsRequired = [qm| At least config name argument is required! |]

    f []     = error cfgTypeIsRequired
    f ["-t"] = error cfgTypeIsRequired

    -- testing flag to test if it's working using config example
    f ("-t" : x : xs) = g True x xs

    f (x : xs) = g False x xs

    g isTesting x xs =
      case configTypeFromStr x of
           Nothing -> error [qm| Unknown config type: "{x}" |]
           Just  a -> (isTesting, a, xs)

    in f <$!> getArgs

  let configFile = configTypeToStr configType <> if isTesting
                                                    then ".cfg.yaml.example"
                                                    else ".cfg.yaml"

  case configType of
       ToolsConfigType -> let
         !lensedEncode = case etcArgs of
           [] -> Aeson.encode
           ["alert_supervisors"] -> Aeson.encode . alert_supervisors
           ["arc_vin_import"]    -> Aeson.encode . arc_vin_import

           [x] -> error [qms|
             Unknown "{configTypeToStr configType :: String}"
             config branch: "{x}"
           |]

           x -> error [qm| Unexpected additional arguments: {x} |]

         in readConfig (Proxy :: Proxy ToolsConfig) lensedEncode configFile

  where
    -- Reads YAML file, parses it as "a" type provided by "Proxy"
    -- and writes JSON of it to stdout.
    -- By second argument it could lense some branch of an "a" type.
    readConfig
      :: (Aeson.FromJSON a, Aeson.ToJSON a)
      => Proxy a -> (a -> BS.ByteString) -> FilePath -> IO ()
    readConfig p@Proxy lensedEncode configFile =
      Yaml.decodeFileEither configFile
        <&!> either fErr (fOk p)
        <&!> lensedEncode
        >>=  BS.putStrLn

      where
        fErr e = error [qms| Error while reading config "{configFile}": {e} |]

        -- "id" with "Proxy" to resolve ambiguous type "a"
        -- (because it must have "ToJSON" implementation).
        fOk :: Proxy a -> a -> a
        fOk Proxy = id


data ConfigType
   = ToolsConfigType
     deriving Eq

configTypeToStr :: (IsString a, Eq a) => ConfigType -> a
configTypeToStr ToolsConfigType = "carma-tools"

configTypeFromStr :: (IsString a, Eq a) => a -> Maybe ConfigType
configTypeFromStr x
  | x == configTypeToStr ToolsConfigType = Just ToolsConfigType
  | otherwise = Nothing
