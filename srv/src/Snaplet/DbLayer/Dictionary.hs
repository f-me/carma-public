{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.Dictionary (
    Dictionary,
    look,
    loadDictionary, loadDictionaries
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import System.FilePath
import System.Directory

data KeyValue = KeyValue {
    key :: T.Text,
    value :: T.Text }
        deriving (Show)

data Dictionary = Dictionary (HM.HashMap T.Text T.Text) | Dictionaries (HM.HashMap T.Text Dictionary)
    deriving (Show)

instance FromJSON KeyValue where
    parseJSON (Object v) = KeyValue <$> (v .: "value") <*> (v .: "label")

instance FromJSON Dictionary where
    parseJSON v@(Array _) = fmap toDict $ parseJSON v where
        toDict :: [KeyValue] -> Dictionary
        toDict = Dictionary . HM.fromList . map (key &&& value)
    parseJSON v@(Object _) = fmap Dictionaries $ parseJSON v
    parseJSON _ = empty

look :: [T.Text] -> Dictionary -> Maybe T.Text
look [] _ = Nothing
look [key] (Dictionary m) = HM.lookup key m <|> HM.lookup "" m
look (key:keys) (Dictionaries m) = do
    d <- HM.lookup key m
    look keys d
look _ _ = Nothing

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
