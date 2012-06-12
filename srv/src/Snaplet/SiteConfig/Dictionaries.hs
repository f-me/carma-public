
{-# LANGUAGE TupleSections #-}

module Snaplet.SiteConfig.Dictionaries
  (loadDictionaries
  ) where

import Data.Functor
import qualified Data.Text as T

import Data.Aeson as Aeson
import System.EasyFile
----------------------------------------------------------------------
import Util (readJSON)


loadDictionaries :: FilePath -> IO Aeson.Value
loadDictionaries cfgDir' = do
  files <- getDirectoryContents cfgDir 
  let jsons = filter ((==".json") . takeExtension) files
  object <$> mapM go jsons
  where
    cfgDir = cfgDir' </> "dictionaries"
    go :: FilePath -> IO (T.Text, Aeson.Value)
    go f = (T.pack $ takeBaseName f,) <$> readJSON (cfgDir </> f)


