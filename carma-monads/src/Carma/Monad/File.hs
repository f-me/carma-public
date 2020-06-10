{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Carma.Monad.File where

import           Prelude hiding (readFile, writeFile)
import qualified Prelude

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified System.Directory (doesFileExist)


type MonadFile m = (Monad m, MonadIO m)

readFile      :: MonadFile m => FilePath -> m String
readFile      = liftIO . Prelude.readFile

writeFile     :: MonadFile m => FilePath -> String -> m ()
writeFile x   = liftIO . Prelude.writeFile x

doesFileExist :: MonadFile m => FilePath -> m Bool
doesFileExist = liftIO . System.Directory.doesFileExist
