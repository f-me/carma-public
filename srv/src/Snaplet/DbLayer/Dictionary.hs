{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snaplet.DbLayer.Dictionary (
    Dictionary,
    look, merge, lookAny, keys,
    loadDictionary, loadDictionaries,
    readRKCCalc
    ) where

import Prelude hiding (log)

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (Result(..))
import qualified Data.Aeson as A (Result(..))
import Data.Maybe
import Data.String
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Vector         as V
import System.FilePath
import System.Directory
import System.Log

import Snaplet.DbLayer.Types

import Util (readJSON)

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

-- | Merge sub-dictionaries
merge :: Dictionary -> Dictionary
merge = Dictionary . merge' where
    merge' :: Dictionary -> [(T.Text, T.Text)]
    merge' (Dictionary m) = m
    merge' (Dictionaries m) = concat $ map merge' $ map snd m

-- | Try look in all subdictionaries
lookAny :: [T.Text] -> Dictionary -> Maybe T.Text
lookAny [] _ = Nothing
lookAny [k] d = look [k] $ merge d
lookAny (k:ks) (Dictionaries m) = do
    d <- lookup k m
    lookAny ks d
lookAny _ _ = Nothing

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

loadDictionaries :: MonadLog m => FilePath -> m Dictionary
loadDictionaries cfg = scope "loadDictionaries" $ do
    contents <- liftIO $ getDirectoryContents cfg
    let
        toName = T.pack . dropExtension . takeFileName
        toFile = (cfg </>)
        isJson = (== ".json") . takeExtension
        (names, files) = unzip . map (toName &&& toFile) . filter isJson $ contents
    ds <- mapM loadDictionary' files
    return $ Dictionaries $ catMaybes $ zipWith (fmap . (,)) names ds
    where
        loadDictionary' :: MonadLog m => FilePath -> m (Maybe Dictionary)
        loadDictionary' f = do
            r <- liftIO $ loadDictionary f
            when (not $ isJust r) $ log Warning $ T.concat ["Unable to load dictionary ", fromString f]
            return r

readRKCCalc :: FilePath -> IO RKCCalc
readRKCCalc cfgDir = do
  c <- readJSON rkcDict
  case fromJSON c :: A.Result RKCCalc of
    A.Error e   -> fail $ "Reading of RKCCalc failed with: " ++ e
    A.Success r -> return r
    where
      rkcDict = cfgDir </> "dictionaries" </> "RKCCalc.json"

instance FromJSON RKCCalc where
  parseJSON (Object o) = do
    Object e <- o .: "entries"
    HM.foldrWithKey f (return Map.empty) e
    where
      f k v m = Map.insert (T.encodeUtf8 k) <$> parseJSON v <*> m
  parseJSON _ = empty

instance FromJSON RKCEntry where
  parseJSON (Array a) = V.foldl f (return Map.empty) a
    where
      f m (Object v) = do
        name  <- v .: "name"
        value <- v .: "value"
        m >>= return  . Map.union (Map.singleton name value)
      f _ _ = empty
  parseJSON _ = empty
