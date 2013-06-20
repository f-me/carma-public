{-# LANGUAGE Rank2Types #-}

{-|

Handle file uploads using @attachment@ model.

-}

module Snaplet.FileUpload
  ( fileUploadInit
  , FileUpload(..)
  , doUpload'
  ) where

import Control.Lens
import Control.Monad

import Data.Aeson as A

import Data.List (foldl')
import Data.String
import Data.Maybe
import Data.Configurator
import Data.ByteString (ByteString)

import System.Directory
import System.FilePath

import Snap (gets, liftIO)
import Snap.Core hiding (path)
import Snap.Snaplet
import Snap.Util.FileUploads

import Snaplet.Auth.Class
import Snaplet.Auth.PGUsers

import Snaplet.DbLayer.Types (DbLayer)

import Utils.HttpErrors
import Util as U

data FileUpload b = FU { cfg      :: UploadPolicy
                       , tmp      :: FilePath
                       , finished :: FilePath
                       , db       :: Lens' b (Snaplet (DbLayer b))
                       }

routes :: [(ByteString, Handler b (FileUpload b) ())]
routes = [ (":model/:id/:field",       method POST   $ doUpload)
         ]

doUpload :: Handler b (FileUpload b) ()
doUpload = do
  model <- getParamOrDie "model"
  objId <- getParamOrDie "id"
  field <- getParamOrDie "field"
  r <- doUpload' model objId field
  -- modifyResponse $ setResponseCode 200
  writeLBS $ A.encode r

doUpload' :: String -> String -> String -> Handler b (FileUpload b) [String]
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
    Nothing -> finishWithError 403 $
               "Required parameter not set: " ++ U.bToString name
    Just p  -> return $ U.bToString p

partPol :: UploadPolicy -> PartUploadPolicy
partPol = allowWithMaximumSize . getMaximumFormInputSize

fileUploadInit :: HasAuth b =>
                  Lens' b (Snaplet (DbLayer b))
               -> SnapletInit b (FileUpload b)
fileUploadInit db =
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
      return $ FU pol tmp finished db
