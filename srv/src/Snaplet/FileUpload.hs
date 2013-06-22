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
  ) where

import Control.Lens
import Control.Monad
import Control.Concurrent.STM

import Data.Aeson as A hiding (Object)
import Data.Attoparsec.Text as P

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

import Utils.HttpErrors
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
routes = [ (":model/bulk/:field",      method POST   $ uploadBulk)
         , (":model/:id/:field",       method POST   $ uploadInField)
         ]

-- | Lift a DbLayer handler action to FileUpload handler.
withDb :: Handler b (DbLayer b) a -> Handler b (FileUpload b) a
withDb = (gets db >>=) . flip withTop

-- | A field of an instance to attach an attachment to.
type AttachmentTarget = (ModelName, ObjectId, FieldName)

-- | Upload a file, create a new attachment (an instance of
-- @attachment@ model), add references to it in a given set of other
-- instance fields. Return the created attachment object and a list of
-- attachment targets used.
--
-- The file is stored under @attachment/<newid>@ directory hierarchy
-- of finished uploads dir.
uploadInManyFields :: (FilePath -> [AttachmentTarget])
                   -- ^ Convert the uploaded file name to a list of
                   -- fields in instances to attach the file to.
                   -> Handler b (FileUpload b) (Object, [AttachmentTarget])
uploadInManyFields flds = do
  -- Create empty attachment instance
  attach <- withDb $ DB.create "attachment" M.empty

  -- Store the file
  let aid = attach M.! "id"
  fPath <- doUpload $ "attachment" </> (B8.unpack aid)

  -- Save filename in attachment
  let fName = takeFileName fPath
  _ <- withDb $ DB.update "attachment" aid $
                M.singleton "filename" (stringToB fName)

  let targets = flds fName
  forM_ targets $ \(model, objId, field) ->
      attachToField model objId field $ B8.append "attachment:" aid

  obj <- withDb $ DB.read "attachment" aid
  return (obj, targets)

-- | Upload and attach a file (as in 'uploadInManyFields'), but read a
-- list of instance ids from the file name (@732,123,452.pdf@; all
-- non-digit characters serve as instance id separators). @model@ and
-- @field@ are read from request parameters.
--
-- Server response is a JSON object with two keys: @attachment@
-- contains an attachment object, @targets@ contains a list of triples
-- with attachment targets used.
uploadBulk :: Handler b (FileUpload b) ()
uploadBulk = do
  -- 'Just' here for these have already been matched by Snap router
  Just model <- getParam "model"
  Just field <- getParam "field"
  res <- uploadInManyFields $
             \fName -> map (\i -> (model, i, field)) (readIds fName)
  writeLBS $ A.encode $ A.object [ "attachment" A..= fst res
                                 , "targets"    A..= snd res]
      where
        -- Read a list of decimal instance id from a file name,
        -- skipping everything else.
        readIds :: FilePath -> [ObjectId]
        readIds fn =
            either (const []) (map $ stringToB . (show :: Int -> String)) $
            parseOnly (many1 $ skipWhile (not . isDigit) >> decimal)
            (T.pack fn)

-- | Upload and attach a file (as in 'uploadInManyFields') to a single
-- instance, given by @model@, @id@ and @field@ request parameters.
uploadInField :: Handler b (FileUpload b) ()
uploadInField = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  Just field <- getParam "field"
  res <- uploadInManyFields $ const [(model, objId, field)]
  writeLBS $ A.encode $ fst res

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
      addRef ""    ref = ref
      addRef field ref = BS.concat [field, ",", ref]
      lockName = BS.concat [modelName, ":", instanceId, "/", field]

-- | Store a file upload from the request using a provided directory
-- (relative to finished uploads path), return full path to the
-- uploaded file.
doUpload :: FilePath -> Handler b (FileUpload b) FilePath
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
      l <- liftIO $ newTVarIO HS.empty
      return $ FU pol tmp finished db l
