
module Snaplet.FileUpload
  ( fileUploadInit
  , FileUpload
  , doUpload'
  , doDeleteAll'
  ) where

import Control.Monad

import Snap (gets, liftIO)
import Snap.Core hiding (path)
import Snap.Snaplet
import Snap.Util.FileUploads

import Data.List (foldl')
import Data.String
import Data.Maybe
import Data.Configurator
import Data.ByteString (ByteString)

import System.Directory
import System.FilePath

import Data.Aeson as A

import Utils.HttpErrors
import Util as U

data FileUpload = FU { cfg      :: UploadPolicy
                     , tmp      :: FilePath
                     , finished :: FilePath
                     }

routes :: [(ByteString, Handler b FileUpload ())]
routes = [ (":model/:id/:field",       method POST   $ doUpload)
         , (":model/:id/:field/:name", method DELETE $ doDelete)
         , (":model/:id",              method DELETE $ doDeleteAll)
         ]

doDeleteAll :: Handler b FileUpload ()
doDeleteAll = do
  model <- getParamOrDie "model"
  objId <- getParamOrDie "id"
  doDeleteAll' model objId

doDeleteAll' :: String -> String -> Handler b FileUpload ()
doDeleteAll' model objId = do
  f <- gets finished
  let path = f </> model </> objId
  when (elem ".." [model, objId]) pass
  liftIO (doesDirectoryExist path) >>= \case
    False -> finishWithError 404 $ model </> objId
    True  -> liftIO $ removeDirectoryRecursive path

doDelete :: Handler b FileUpload ()
doDelete = do
  f     <- gets finished
  model <- getParamOrDie "model"
  objId <- getParamOrDie "id"
  field <- getParamOrDie "field"
  name  <- getParamOrDie "name"

  let path' = [model, objId, field, name]
  when (elem ".." path') pass
  let path  = foldl' (</>) f path'
  liftIO (doesFileExist path) >>= \case
    False -> finishWithError 404 name
    True  -> do
      liftIO $ removeFile path
      writeText $ fromString name

doUpload :: Handler b FileUpload ()
doUpload = do
  model <- getParamOrDie "model"
  objId <- getParamOrDie "id"
  field <- getParamOrDie "field"
  r <- doUpload' model objId field
  -- modifyResponse $ setResponseCode 200
  writeLBS $ A.encode r

doUpload' :: String -> String -> String -> Handler b FileUpload [String]
doUpload' model objId field = do
  tmpd <- gets tmp
  cfg  <- gets cfg
  f    <- gets finished
  handleFileUploads tmpd cfg (const $ partPol cfg) $
    liftIO . fmap catMaybes . mapM (\(info, r) -> case r of
      Left _    -> return Nothing
      Right res -> do
        let justFname = U.bToString . fromJust $ partFileName info
        let path      = f </> model </> objId </> field
        createDirectoryIfMissing True path
        copyFile res $ path </> justFname
        return $ Just justFname)

getParamOrDie name =
  getParam name >>= \case
    Nothing -> finishWithError 403 $ "need " ++ U.bToString name
    Just p  -> return $ U.bToString p

partPol :: UploadPolicy -> PartUploadPolicy
partPol = allowWithMaximumSize . getMaximumFormInputSize

fileUploadInit :: SnapletInit b FileUpload
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
          pol      = setProcessFormInputs         True
                     $ setMaximumFormInputSize maxFile'
                     -- $ setMaximumNumberOfFormInputs maxInp
                     $ setMinimumUploadRate    minRate'
                     $ setMinimumUploadSeconds kickLag
                     $ setUploadTimeout        inact
                       defaultUploadPolicy
      addRoutes routes
      return $ FU pol tmp finished
