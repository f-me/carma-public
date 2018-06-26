{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.Monad.File
     ( MonadFile (..)
     ) where

import           Prelude hiding (readFile, writeFile)
import qualified Prelude

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified System.Directory (doesFileExist)


class Monad m => MonadFile m where
  readFile      :: FilePath -> m String
  writeFile     :: FilePath -> String -> m ()
  doesFileExist :: FilePath -> m Bool


instance (Monad m, MonadIO m) => MonadFile m where
  readFile      = liftIO . Prelude.readFile
  writeFile x   = liftIO . Prelude.writeFile x
  doesFileExist = liftIO . System.Directory.doesFileExist
