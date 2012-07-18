{-# LANGUAGE TemplateHaskell #-}
module Snaplet.FileUpload (fileUploadInit, FileUpload)
where

import Control.Monad.IO.Class
import Control.Monad

import Snap (gets)
import Snap.Core
import Snap.Snaplet
import Snap.Util.FileUploads

import System.IO

import Data.Maybe
import Data.Lens.Template
import Data.Configurator
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8  as BU
import qualified Codec.Binary.UTF8.String as UTF8

import System.Directory
import System.FilePath

import Data.Aeson as A

import Utils.HttpErrors

data FileUpload = FU { cfg      :: UploadPolicy
                     , tmp      :: FilePath
                     , finished :: FilePath
                     }

makeLens ''FileUpload

routes = [ (":model/:id/:field", method POST $ doUpload)
         , (":model/:id/:field/:name", method DELETE $ doDelete) ]

doDelete :: Handler b FileUpload ()
doDelete = do
  f     <- gets finished
  model <- getParamOrDie "model"
  id    <- getParamOrDie "id"
  field <- getParamOrDie "field"
  name  <- getParamOrDie "name"
  let splited = map (BU.toString) [model, id, field, name]
      path    = foldl (</>) f splited
  when (elem ".." splited) pass
  e <- liftIO $ doesFileExist path
  when (not e) $ finishWithError 404 name
  liftIO $ removeFile path
  writeBS name

doUpload :: Handler b FileUpload ()
doUpload = do
  tmpd <- gets tmp
  cfg  <- gets cfg
  f    <- gets finished
  model <- getParamOrDie "model"
  id    <- getParamOrDie "id"
  field <- getParamOrDie "field"
  r <- handleFileUploads tmpd cfg (partPol cfg) (uploadHandler f model id field)
  -- modifyResponse $ setResponseCode 200
  writeLBS $ A.encode r

getParamOrDie name =
  getParam name >>=
  maybe (finishWithError 403 $ "need " `B.append` name) return

partPol cfg _ = allowWithMaximumSize $ getMaximumFormInputSize cfg

uploadHandler f model id field l =
    liftIO $ liftM (catMaybes) $ mapM (handle f model id field) l

handle _ _     _  _     (_   , Left _ ) = return Nothing
handle d model id field (info, Right f) = do
  createDirectoryIfMissing True path
  copyFile f $ path </> justFname
  return $ Just justFname
    where
      justFname = BU.toString $ fromJust $ partFileName info
      path      = d </> B.unpack model </> B.unpack id </> B.unpack field

fileUploadInit =
    makeSnaplet "fileupload" "fileupload" Nothing $ do
      cfg      <- getSnapletUserConfig
      maxFile  <- liftIO $ lookupDefault 100  cfg "max-file-size"
      -- maxInp   <- liftIO $ lookupDefault 10   cfg "max-form-inputs"
      minRate  <- liftIO $ lookupDefault 1000 cfg "min-upload-rate"
      kickLag  <- liftIO $ lookupDefault 10   cfg "min-rate-kick-lag"
      inact    <- liftIO $ lookupDefault 20   cfg "inactivity-timeout"
      tmp      <- liftIO $ require            cfg "tmp-path"
      finished <- liftIO $ require            cfg "finished-path"
      -- we need some values in bytes
      let maxFile' = maxFile * 1024
          minRate' = minRate * 1024
          pol      = setProcessFormInputs         False
                     $ setMaximumFormInputSize maxFile'
                     -- $ setMaximumNumberOfFormInputs maxInp
                     $ setMinimumUploadRate    minRate'
                     $ setMinimumUploadSeconds kickLag
                     $ setUploadTimeout        inact
                       defaultUploadPolicy
      addRoutes routes
      return $ FU pol tmp finished
