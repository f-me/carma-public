{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Snaplet.DbLayer.Dictionary (
    Dictionary,
    look, merge, lookAny, keys,
    loadDictionary, loadDictionaries,
    readRKCCalc
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Vector         as V
import System.FilePath
import System.Directory

import Snaplet.DbLayer.Types

data KeyValue = KeyValue {
    key :: T.Text,
    value :: T.Text }
        deriving (Show)

data Dictionary = Dictionary (HM.HashMap T.Text T.Text) | Dictionaries (HM.HashMap T.Text Dictionary)
    deriving (Show)

instance FromJSON KeyValue where
    parseJSON (Object v) = KeyValue <$> (v .: "value") <*> (v .: "label")
    parseJSON _ = empty

instance FromJSON Dictionary where
    parseJSON v@(Array _) = fmap toDict $ parseJSON v where
        toDict :: [KeyValue] -> Dictionary
        toDict = Dictionary . HM.fromList . map (key &&& value)
    parseJSON v@(Object _) = fmap Dictionaries $ parseJSON v
    parseJSON _ = empty

look :: [T.Text] -> Dictionary -> Maybe T.Text
look [] _ = Nothing
look [k] (Dictionary m) = HM.lookup k m <|> HM.lookup "" m
look (k:ks) (Dictionaries m) = do
    d <- HM.lookup k m
    look ks d
look _ _ = Nothing

-- | Merge sub-dictionaries
merge :: Dictionary -> Dictionary
merge = Dictionary . merge' where
    merge' :: Dictionary -> HM.HashMap T.Text T.Text
    merge' (Dictionary m) = m
    merge' (Dictionaries m) = HM.unions $ map merge' $ HM.elems m

-- | Try look in all subdictionaries
lookAny :: [T.Text] -> Dictionary -> Maybe T.Text
lookAny [] _ = Nothing
lookAny [k] d = look [k] $ merge d
lookAny (k:ks) (Dictionaries m) = do
    d <- HM.lookup k m
    lookAny ks d
lookAny _ _ = Nothing

keys :: [T.Text] -> Dictionary -> Maybe [T.Text]
keys [] (Dictionary m) = Just $ HM.keys m
keys [] (Dictionaries m) = Just $ HM.keys m
keys _ (Dictionary _) = Nothing
keys (k:ks) (Dictionaries m) = do
    d <- HM.lookup k m
    keys ks d

loadDictionary :: FilePath -> IO (Maybe Dictionary)
loadDictionary f = fmap (decode >=> unEntries) $ LC8.readFile f where
    unEntries :: Dictionary -> Maybe Dictionary
    unEntries (Dictionaries ds)
        | HM.keys ds == ["entries"] = HM.lookup "entries" ds
        | otherwise = Nothing
    unEntries d = Just d

loadDictionaries :: FilePath -> IO Dictionary
loadDictionaries cfg = do
    contents <- getDirectoryContents cfg
    let
        toName = T.pack . dropExtension . takeFileName
        toFile = (cfg </>)
        isJson = (== ".json") . takeExtension
        (names, files) = unzip . map (toName &&& toFile) . filter isJson $ contents
    ds <- mapM loadDictionary files
    return $ Dictionaries $ HM.fromList $ catMaybes $ zipWith (fmap . (,)) names ds

readRKCCalc :: FilePath -> IO RKCCalc
readRKCCalc cfgDir = do
  c <- LC8.readFile rkcDict
  return Map.empty
    where
      rkcDict = cfgDir </> "dictionaries" </> "RKCCalc.json"

instance FromJSON RKCCalc where
  parseJSON (Object o) = do
    Object e <- o .: "entries"
    HM.foldrWithKey f (return Map.empty) e
    where
      f k v m = Map.insert (T.encodeUtf8 k) <$> parseJSON v <*> m

instance FromJSON RKCEntry where
  parseJSON (Array a) = V.foldl f (return Map.empty) a
    where
      f m (Object v) = do
        name  <- v  .: "name"
        value <- v .: "value"
        m >>= return  . Map.union (Map.singleton name value)
