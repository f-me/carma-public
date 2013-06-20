{-# LANGUAGE Rank2Types #-}

{-|

Handle file uploads using @attachment@ model.

TODO: Handle @attachment@ model permissions in upload handlers.

-}

module Snaplet.FileUpload
  ( fileUploadInit
  , FileUpload(..)
  , doUpload
  ) where

import Control.Lens

import Data.Aeson as A

import qualified Data.Map as M
import Data.Maybe
import Data.Configurator
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import System.Directory
import System.FilePath

import Snap (gets, liftIO)
import Snap.Core hiding (path)
import Snap.Snaplet
import Snap.Util.FileUploads

import Snaplet.Auth.Class
import Snaplet.Auth.PGUsers

import qualified Snaplet.DbLayer as DB
import Snaplet.DbLayer.Types (DbLayer)

import Utils.HttpErrors
import Util as U

data FileUpload b = FU { cfg      :: UploadPolicy
                       , tmp      :: FilePath
                       , finished :: FilePath
                       -- ^ Root directory of finished uploads.
                       , db       :: Lens' b (Snaplet (DbLayer b))
                       }

routes :: [(ByteString, Handler b (FileUpload b) ())]
routes = [ (":model/:id/:field",       method POST   $ attachToField)
         ]


-- | Lift DbLayer handler action to FileUpload handler.
withDb :: Handler b (DbLayer b) a -> Handler b (FileUpload b) a
withDb = (gets db >>=) . flip withTop


-- | Create a new attachment (an instance of @attachment@ model) and
-- add a reference to it in a field of another model instance, set by
-- @model@, @id@ and @field@ request parameters. Serve JSON with
-- @attachment@ instance data in response.
attachToField :: Handler b (FileUpload b) ()
attachToField = do
  model <- getParamOrDie "model"
  objId <- getParamOrDie "id"
  field <- getParamOrDie "field"
  fName <- doUpload $ model </> objId </> field
  let parentRef = BS.concat [stringToB model, ":", stringToB objId]
  attach <- withDb $
            DB.create "attachment" $
            M.fromList [ ("filename", stringToB fName)
                       , ("parentRef", parentRef)
                       ]
  -- modifyResponse $ setResponseCode 200
  writeLBS $ A.encode attach

-- | Store a file upload from the request using a provided directory
-- (relative to finished uploads path), return its file name.
--
-- Full path to the uploaded file may be obtained as follows (@fu@ is
-- a lens to a FileUpload snaplet):
--
-- > fName <- with fu $ doUpload relPath
-- > finishedRoot <- with fu $ gets finished
-- > let path = finishedRoot </> relPath </> fName
doUpload :: FilePath -> Handler b (FileUpload b) String
doUpload relPath = do
  tmpd <- gets tmp
  cfg  <- gets cfg
  f    <- gets finished
  fns  <- handleFileUploads tmpd cfg (const $ partPol cfg) $
    liftIO . fmap catMaybes . mapM (\(info, r) -> case r of
      Left _    -> return Nothing
      Right res -> do
        let justFname = U.bToString . fromJust $ partFileName info
        let path      = f </> relPath
        createDirectoryIfMissing True path
        copyFile res $ path </> justFname
        return $ Just justFname)
  return $ head fns

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
      minRate  <- liftIO $ lookupDefault 1000 cfg "min-upload-rate"
      kickLag  <- liftIO $ lookupDefault 10   cfg "min-rate-kick-lag"
      inact    <- liftIO $ lookupDefault 20   cfg "inactivity-timeout"
      tmp      <- liftIO $ require            cfg "tmp-path"
      finished <- liftIO $ require            cfg "finished-path"
      -- we need some values in bytes
      let maxFile' = maxFile * 1024
          minRate' = minRate * 1024
          -- Every thread is for a single file
          maxInp   = 1
          pol      = setProcessFormInputs         True
                     $ setMaximumFormInputSize maxFile'
                     $ setMaximumNumberOfFormInputs maxInp
                     $ setMinimumUploadRate    minRate'
                     $ setMinimumUploadSeconds kickLag
                     $ setUploadTimeout        inact
                       defaultUploadPolicy
      addRoutes routes
      return $ FU pol tmp finished db
