module Snaplet.DbLayer.Dictionary (
	dictionary, loadMap, loadMaps, loadDicts
	) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Map as M
import qualified Data.Text as T
import System.FilePath
import System.Directory

data KeyValue = KeyValue {
    key :: T.Text,
    value :: T.Text }
        deriving (Show)

data Dictionary = Dictionary {
    entries :: [KeyValue] }
        deriving (Show)

instance FromJSON KeyValue where
    parseJSON (Object v) = KeyValue <$> (v .: (T.pack "value")) <*> (v .: (T.pack "label"))

instance FromJSON Dictionary where
    parseJSON (Object v) = Dictionary <$> (v .: (T.pack "entries"))

dictionary :: Dictionary -> M.Map String String
dictionary = M.fromList . map ((T.unpack . key) &&& (T.unpack . value)) . entries

loadMap :: FilePath -> IO (M.Map String String)
loadMap = fmap (maybe M.empty dictionary) . loadDictionary

loadMaps :: FilePath -> [String] -> IO (M.Map String (M.Map String String))
loadMaps f ds = fmap M.fromList $ forM ds $ \d -> do
    m <- loadMap (f </> (d ++ ".json"))
    return (d, m)

loadDictionary :: FilePath -> IO (Maybe Dictionary)
loadDictionary f = fmap decode $ LC8.readFile f

loadValue :: FilePath -> IO (Maybe Value)
loadValue f = fmap decode $ LC8.readFile f

loadDicts :: FilePath -> IO (M.Map String (M.Map String String))
loadDicts cfg = do
    files <- getDirectoryContents cfg
    let dictNames = map (dropExtension . takeFileName) $ filter ((== ".json") . takeExtension) files
    loadMaps cfg dictNames
