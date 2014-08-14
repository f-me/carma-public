{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snaplet.DbLayer.Dictionary (
    Dictionary,
    look, keys,
    loadDictionary, loadDictionaries
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (Result(..))
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text           as T
import qualified Data.HashMap.Strict as HM
import System.FilePath
import System.Directory

import Util (syslogTxt, Priority(..))

data KeyValue = KeyValue {
    key :: T.Text,
    value :: T.Text }
        deriving (Show)

data Dictionary = Dictionary [(T.Text, T.Text)] | Dictionaries [(T.Text, Dictionary)]
    deriving (Show)

instance FromJSON KeyValue where
    parseJSON (Object v) = KeyValue <$> (v .: "value") <*> (v .: "label")
    parseJSON _ = empty

instance FromJSON Dictionary where
    parseJSON v@(Array _) = fmap toDict $ parseJSON v where
        toDict :: [KeyValue] -> Dictionary
        toDict = Dictionary . map (key &&& value)
    parseJSON v@(Object _) = fmap (Dictionaries . HM.toList) $ parseJSON v
    parseJSON _ = empty

look :: [T.Text] -> Dictionary -> Maybe T.Text
look [] _ = Nothing
look [k] (Dictionary m) = lookup k m <|> lookup "" m
look (k:ks) (Dictionaries m) = do
    d <- lookup k m
    look ks d
look _ _ = Nothing

keys :: [T.Text] -> Dictionary -> Maybe [T.Text]
keys [] (Dictionary m) = Just $ map fst m
keys [] (Dictionaries m) = Just $ map fst m
keys _ (Dictionary _) = Nothing
keys (k:ks) (Dictionaries m) = do
    d <- lookup k m
    keys ks d

loadDictionary :: FilePath -> IO (Maybe Dictionary)
loadDictionary f = fmap (decode >=> unEntries) $ LC8.readFile f where
    unEntries :: Dictionary -> Maybe Dictionary
    unEntries (Dictionaries ds)
        | map fst ds == ["entries"] = lookup "entries" ds
        | otherwise = Nothing
    unEntries d = Just d

loadDictionaries :: MonadIO m => FilePath -> m Dictionary
loadDictionaries cfg = do
    contents <- liftIO $ getDirectoryContents cfg
    let
        toName = T.pack . dropExtension . takeFileName
        toFile = (cfg </>)
        isJson = (== ".json") . takeExtension
        (names, files) = unzip . map (toName &&& toFile) . filter isJson $ contents
    ds <- mapM loadDictionary' files
    return $ Dictionaries $ catMaybes $ zipWith (fmap . (,)) names ds
    where
        loadDictionary' :: MonadIO m => FilePath -> m (Maybe Dictionary)
        loadDictionary' f = do
            r <- liftIO $ loadDictionary f
            when (not $ isJust r)
              $ syslogTxt Error "loadDictionaries" $ concat ["Unable to load dictionary ", f]
            return r
