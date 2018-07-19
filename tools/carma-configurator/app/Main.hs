{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Data.Proxy
import           Data.Monoid ((<>))
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
  (configType, etcArgs) <-
    getArgs <&!> \case
      []     -> error [qm| At least config name argument is required! |]
      (x:xs) -> case configTypeFromStr x of
                     Nothing -> error [qm| Unknown config type: "{x}" |]
                     Just  a -> (a, xs)

  let configFile = configTypeToStr configType <> ".cfg.yaml"

  case configType of
       ToolsConfigType -> do
         let !lensedEncode = case etcArgs of
               [] -> Aeson.encode
               ["alert_supervisors"] -> Aeson.encode . alert_supervisors

               [x] -> error [qms|
                 Unknown "{configTypeToStr configType :: String}"
                 config branch: "{x}"
               |]

               x -> error [qm| Unexpected additional arguments: {x} |]

         readConfig (Proxy :: Proxy ToolsConfig) lensedEncode configFile

  where
    -- Reads YAML file, parses it as "a" type provided by "Proxy"
    -- and writes JSON of it to stdout.
    -- By second argument it could lense some branch of an "a" type.
    readConfig
      :: (Aeson.FromJSON a, Aeson.ToJSON a)
      => Proxy a -> (a -> BS.ByteString) -> FilePath -> IO ()
    readConfig p@Proxy lensedEncode configFile =
      Yaml.decodeFileEither configFile
        <&> either fErr (fOk p)
        <&> lensedEncode
        >>= BS.putStrLn

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
