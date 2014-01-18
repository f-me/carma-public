{-# LANGUAGE ScopedTypeVariables #-}

module Carma.VIN
    ( vinImport
    )

where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Error as E
import           Control.Monad.Trans.Reader

import           Data.Model
import           Data.Model.Patch     as Patch
import           Carma.Model.VinFormat

import           Carma.VIN.Base


getOptions :: (Options -> a) -> Import a
getOptions proj = lift $ lift $ asks proj


throwError :: ImportError -> Import a
throwError err = lift $ E.throwError err

vinImport :: Import ()
vinImport = do
  let vfInfo = modelInfo :: ModelInfo VinFormat
  v <- asks vinFormat
  liftIO $ print $ map fd_name $ modelFields vfInfo
  liftIO $ forM_ vinFormatAccessors $ 
             \(VFAcc (CF p) (LoadAcc l) (RequiredAcc r)) -> do
               print $ fieldName p
               print $ Patch.get v l
               print $ Patch.get v r
