{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DoAndIfThenElse #-}

{-|

Handle file uploads using @attachment@ model.

TODO: Handle @attachment@ model permissions in upload handlers.

-}

module Snaplet.FileUpload
  ( fileUploadInit
  , FileUpload(..)
  , doUpload
  , getAttachmentPath
  ) where

import Control.Lens
import Control.Monad
import Control.Concurrent.STM

import Data.Aeson as A hiding (Object)
import Data.Attoparsec.Text as P

import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Data.Configurator
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashSet as HS
import Data.Char
import qualified Data.Text as T

import System.Directory
import System.FilePath

import Snap (gets, liftIO)
import Snap.Core hiding (path)
import Snap.Snaplet
import Snap.Util.FileUploads

import Snaplet.Auth.Class
import Snaplet.Auth.PGUsers

import qualified Snaplet.DbLayer as DB
import Snaplet.DbLayer.Types

import Util as U

data FileUpload b = FU { cfg      :: UploadPolicy
                       , tmp      :: FilePath
                       , finished :: FilePath
                       -- ^ Root directory of finished uploads.
                       , db       :: Lens' b (Snaplet (DbLayer b))
                       , locks    :: TVar (HS.HashSet ByteString)
                       -- ^ Set of references to currently locked
                       -- instances.
                       }

routes :: [(ByteString, Handler b (FileUpload b) ())]
routes = [ (":model/bulk/:field",      method POST uploadBulk)
         , (":model/:id/:field",       method POST uploadInField)
         ]

-- | Lift a DbLayer handler action to FileUpload handler.
withDb :: Handler b (DbLayer b) a -> Handler b (FileUpload b) a
withDb = (gets db >>=) . flip withTop

-- | A field of an instance to attach an attachment to.
type AttachmentTarget = (ModelName, ObjectId, FieldName)

-- | Upload a file, create a new attachment (an instance of
-- @attachment@ model), add references to it in a given set of other
-- instance fields (attachment targets) which depend on the filename.
-- Rename the saved file after reading attachment targets. Return the
-- created attachment object, lists of failed (left) and successful
-- (right) attachment targets. Target is unsuccessful if a referenced
-- instance does not exist.
--
-- The file is stored under @attachment/<newid>@ directory hierarchy
-- of finished uploads dir.
uploadInManyFields :: (FilePath -> [AttachmentTarget])
                   -- ^ Convert the uploaded file name to a list of
                   -- fields in instances to attach the file to.
                   -> Maybe (FilePath -> FilePath)
                   -- ^ Change source file name when saving.
                   -> Handler b (FileUpload b)
                      (Object, [AttachmentTarget], [AttachmentTarget])
uploadInManyFields flds nameFun = do
  -- Create empty attachment instance
  attach <- withDb $ DB.create "attachment" M.empty

  -- Store the file
  let aid = attach M.! "id"
  fPath <- doUpload ("attachment" </> B8.unpack aid)

  -- Save new filename in attachment
  let (fDir, fName) = splitFileName fPath
      newName = (fromMaybe id nameFun) fName
  liftIO $ renameFile fPath (fDir </> newName)
  _ <- withDb $ DB.update "attachment" aid $
                M.singleton "filename" (stringToB newName)

  -- Attach to target field for existing instances, using the original
  -- filename
  let targets = flds fName
  results <-
      forM targets $ \t@(model, objId, field) -> do
          e <- withDb $ DB.exists model objId
          if e
          then do
              attachToField model objId field $ B8.append "attachment:" aid
              return $ Right t
          else
              return $ Left t
  let (failedTargets, succTargets) = partitionEithers results

  -- Serve back full attachment instance
  obj <- withDb $ DB.read "attachment" aid

  return (obj, failedTargets, succTargets)

-- | Upload and attach a file (as in 'uploadInManyFields'), but read a
-- list of instance ids from the file name (@732,123,452-foo09.pdf@
-- reads to @[732, 123, 452]@; all non-digit characters serve as
-- instance id separators, no number past the first @-@ character are
-- read). @model@ and @field@ are read from request parameters.
--
-- Server response is a JSON object with three keys: @attachment@
-- contains an attachment object, @targets@ contains a list of triples
-- with attachment targets used, @unknown@ is a failed attachment
-- target list.
uploadBulk :: Handler b (FileUpload b) ()
uploadBulk = do
  -- 'Just' here for these have already been matched by Snap router
  Just model <- getParam "model"
  Just field <- getParam "field"
  (obj, failedTargets, succTargets) <-
      uploadInManyFields
             (\fName -> map (\i -> (model, i, field)) (readIds fName))
             (Just cutIds)
  writeLBS $ A.encode $ A.object [ "attachment" A..= obj
                                 , "targets"    A..= succTargets
                                 , "unknown"    A..= failedTargets
                                 ]
      where
        -- Read a list of decimal instance ids from a file name,
        -- skipping everything else.
        readIds :: FilePath -> [ObjectId]
        readIds fn =
            either (const []) (map $ stringToB . (show :: Int -> String)) $
            parseOnly (manyTill
                       (skipWhile (not . isDigit) >> decimal)
                       (char '-'))
            (T.pack fn)
        -- Cut out all ids from a filename prior to the first dash char.
        cutIds :: FilePath -> FilePath
        cutIds fp = if elem '-' fp
                    then tail $ dropWhile (/= '-') fp
                    else fp

-- | Upload and attach a file (as in 'uploadInManyFields') to a single
-- instance, given by @model@, @id@ and @field@ request parameters.
uploadInField :: Handler b (FileUpload b) ()
uploadInField = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  Just field <- getParam "field"
  (res, _, _) <- uploadInManyFields (const [(model, objId, field)]) Nothing
  writeLBS $ A.encode $ res


-- | Return path to an attached file (prepended by finished uploads
-- dir).
getAttachmentPath :: ObjectId
                  -- ^ Attachment ID.
                  -> Handler b (FileUpload b) FilePath
getAttachmentPath aid = do
  obj <- withDb $ DB.read "attachment" aid
  fPath <- gets finished
  case M.lookup "filename" obj of
    Just fName -> return $
                  fPath </> "attachment" </> 
                  B8.unpack aid </> B8.unpack fName
    _ -> error $ "Broken attachment" ++ B8.unpack aid

-- | Append a reference of form @attachment:213@ to a field of another
-- instance. This handler is thread-safe.
attachToField :: ModelName
              -- ^ Name of target instance model.
              -> ObjectId
              -- ^ Id of target instance.
              -> FieldName
              -- ^ Field name in target instance.
              -> ByteString
              -- ^ A reference to an attachment instance to be added
              -- in a field of target instance.
              -> Handler b (FileUpload b) ()
attachToField modelName instanceId field ref = do
  l <- gets locks
  -- Lock the field or wait for lock release
  liftIO $ atomically $ do
    hs <- readTVar l
    if HS.member lockName hs
    then retry
    else writeTVar l (HS.insert lockName hs)
  -- Append new ref to the target field
  inst <- withDb $ DB.read modelName instanceId
  let newRefs = addRef (M.findWithDefault "" field inst) ref
  _  <- withDb $ DB.update modelName instanceId $ M.insert field newRefs inst
  -- Unlock the field
  liftIO $ atomically $ do
    hs <- readTVar l
    writeTVar l (HS.delete lockName hs)
  return ()
    where
      addRef ""    r = r
      addRef val   r = BS.concat [val, ",", r]
      lockName = BS.concat [modelName, ":", instanceId, "/", field]

-- | Store a file upload from the request, return full path to the
-- uploaded file.
doUpload :: FilePath
         -- ^ Store a file in this directory (relative to finished
         -- uploads path)
         -> Handler b (FileUpload b) FilePath
doUpload relPath = do
  tmpd <- gets tmp
  cfg  <- gets cfg
  root <- gets finished
  fns  <- handleFileUploads tmpd cfg (const $ partPol cfg) $
    liftIO . fmap catMaybes . mapM (\(info, r) -> case r of
      Left _    -> return Nothing
      Right res -> do
        let justFname = U.bToString . fromJust $ partFileName info
        let path      = root </> relPath
        createDirectoryIfMissing True path
        copyFile res $ path </> justFname
        return $ Just justFname)
  return $ root </> relPath </> head fns

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
      l <- liftIO $ newTVarIO HS.empty
      return $ FU pol tmp finished db l
