
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DictionaryCache where


import Control.Applicative

import Data.Text (Text)
import qualified Data.ByteString.Lazy as L

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson as A
import Data.Aeson.TH

import System.FilePath


data DictCache = DictCache 
  {city        :: Map Text Text
  ,wazzup      :: Map Text Text
  ,carMake     :: Map Text Text
  ,carModel    :: Map Text (Map Text Text)
  ,smsToken    :: Map Text Text
  ,smsTokenVal :: Map Text (Map Text Text)
  ,cancelReason:: Map Text Text
  }


loadDictionaries :: FilePath -> IO DictCache
loadDictionaries dir = do
  DictCache
    <$> flatDict (dir </> "DealerCities.json")
    <*> flatDict (dir </> "Wazzup.json")
    <*> flatDict (dir </> "CarMakers.json")
    <*> nestDict (dir </> "CarModels.json")
    <*> flatDict (dir </> "SmsTokens.json")
    <*> nestDict (dir </> "SmsTokensValues.json")
    <*> flatDict (dir </> "ClientCancelReason.json")


flatDict :: FilePath -> IO (Map Text Text)
flatDict fp = do
  dic <- A.decode' <$> L.readFile fp
  case dic of
    Nothing -> error "can't parse dictionary"
    Just (FlatDict es) -> return $ keyValMap es


nestDict :: FilePath -> IO (Map Text (Map Text Text))
nestDict fp = do
  dic <- A.decode' <$> L.readFile fp
  case dic of
    Nothing -> error "can't parse dictionary"
    Just (NestDict mp) -> return $ Map.map keyValMap mp


keyValMap :: [KeyVal] -> Map Text Text
keyValMap kvs = Map.fromList [(value x, label x) | x <-  kvs]


data KeyVal = KeyVal {value :: Text, label :: Text}
data FlatDict = FlatDict {f'entries :: [KeyVal]}
data NestDict = NestDict {n'entries :: Map Text [KeyVal]}

$(deriveFromJSON defaultOptions ''KeyVal)
$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 2} ''FlatDict)
$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 2} ''NestDict)

