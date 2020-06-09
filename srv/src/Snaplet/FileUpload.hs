{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

File upload helpers and @attachment@ model handling.

File are attached using old-style references (@Attachment:231@).

TODO Use @attachment@ model permissions in upload handlers.

-}

module Snaplet.FileUpload
    ( fileUploadInit
    , FileUpload(..)
    , doUpload
    , doUploadTmp
    , withUploads
    , oneUpload
    , getAttachmentPath
    )

where

import BasicPrelude hiding (show)
import Prelude (show)

import Control.Concurrent.STM
import Control.Monad.State.Class

import Data.Aeson as A hiding (Object)
import Data.Attoparsec.Text as P

import Data.Maybe
import Data.Configurator
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.SHA (sha256)
import qualified Data.HashSet as HS
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.SqlQQ
import Data.Char
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable

import GHC.TypeLits

import System.Directory
import System.FilePath
import System.IO

import Snap.Core hiding (path)
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple hiding (field)
import Snap.Util.FileUploads

import Carma.Model
import Data.Model
import Carma.Model.LegacyTypes
import qualified Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as DB
import Data.Model.Utils.LegacyModel

import Carma.Model.Attachment as Attachment
import Carma.Utils.Snap



data FileUpload b = FU { cfg      :: UploadPolicy
                       , tmp      :: FilePath
                       , finished :: FilePath
                       -- ^ Root directory of finished uploads.
                       , locks    :: TVar (HS.HashSet Text)
                       -- ^ Set of references to currently locked
                       -- instances.
                       , pg       :: SnapletLens b Postgres
                       }


routes :: [(ByteString, Handler b (FileUpload b) ())]
routes = [ (":model/bulk/:field",      method POST uploadBulk)
         , (":model/:id/:field",       method POST uploadInField)
         ]


-- | SQL query used to select attachment id by hash. Parametrized by
-- hash value.
hashToAid :: Query
hashToAid = [sql|SELECT id FROM attachmenttbl WHERE hash=?;|]


-- | Attachment target: a model name, id and a field attach a file to.
type ATarget = (Text, Int, Text)


-- | Upload a file, create a new attachment (an instance of
-- @attachment@ model), add references to it in a given set of other
-- instance fields (attachment targets) which depend on the filename.
-- Rename the saved file after reading attachment targets. Return the
-- created attachment object, lists of failed (left) and successful
-- (right) attachment targets, and a flag indicating that the file was
-- recognized as a duplicate. Target is unsuccessful if a referenced
-- instance does not exist.
--
-- The file is stored under @attachment/<newid>@ directory hierarchy
-- of finished uploads dir.
uploadInManyFields :: (FilePath -> [ATarget])
                   -- ^ Convert the uploaded file name to a list of
                   -- fields in instances to attach the file to.
                   -> Maybe (FilePath -> FilePath)
                   -- ^ Change source file name when saving.
                   -> Handler b (FileUpload b)
                      (Patch.Object Attachment, [ATarget], [ATarget], Bool)
uploadInManyFields flds nameFun = do
  -- Store the file
  fPath <- oneUpload =<< doUpload =<< gets tmp
  let (_, fName) = splitFileName fPath

  fileHash <- liftIO $ sha256 <$> BL.readFile fPath
  now <- liftIO $ getCurrentTime

  root <- gets finished
  (attach@(Ident aid), dupe) <- withLens pg $ liftPG' $ \conn -> do
    -- Check for duplicate files
    res <- PS.query conn hashToAid (Only $ show fileHash)
    case res of
      (Only aid:_) -> return (aid, True)
      [] ->
        let
          newName = (fromMaybe id nameFun) fName
          patch   = Patch.put Attachment.ctime now $
                    Patch.put Attachment.filename (T.pack newName) $
                    Patch.put Attachment.hash (T.pack $ show fileHash) $
                    Patch.empty
        in
          -- Create empty attachment instance
          DB.create patch conn >>=
          \case
            Left e -> error $ show e
            Right attach@(Ident aid) -> do
              -- Move file to attachment/<aid>
              let newDir = root </> "attachment" </> show aid
              createDirectoryIfMissing True newDir >>
                copyFile fPath (newDir </> newName) >>
                removeFile fPath
              return (attach, False)

  -- Attach to target field for existing instances, using the original
  -- filename
  let targets = flds fName
  results <-
    forM targets $
    \t@(model, objId, field) ->
      let
        attachToModel :: forall m b. (Model m) =>
                         m
                      -> Handler b (FileUpload b) (Either ATarget ATarget)
        attachToModel _ =
          let
            mIdent = Ident objId :: IdentI m
          in
            (withLens pg $ liftPG' $ DB.exists mIdent) >>=
            \case
              Right True -> do
                attachToField mIdent field $
                  T.concat $ [ modelName (modelInfo :: ModelInfo Attachment)
                             , ":"
                             , T.pack $ show aid
                             ]
                return $ Right t
              _ -> return $ Left t
      in
        case dispatch model attachToModel of
          Just t' -> t'
          Nothing -> return $ Left t
  let (failedTargets, succTargets) = partitionEithers results

  -- Serve back full attachment instance
  obj <- either (\e ->
                   error $ "Could not read attachment back: " ++ show e) id <$>
         (withLens pg $ liftPG' $ DB.read attach)

  return (obj, failedTargets, succTargets, dupe)


-- | Upload and attach a file (as in 'uploadInManyFields'), but read a
-- list of instance ids from the file name (@732,123,452-foo09.pdf@
-- reads to @[732, 123, 452]@; all non-digit characters serve as
-- instance id separators, no number past the first @-@ character are
-- read). @model@ and @field@ are read from request parameters.
--
-- Server response is a JSON object with four keys: @attachment@
-- contains an attachment object, @targets@ contains a list of triples
-- with attachment targets used, @unknown@ is a failed attachment
-- target list, @dupe@ is true if the file was a duplicate.
uploadBulk :: Handler b (FileUpload b) ()
uploadBulk = do
  -- 'Just' here, for these have already been matched by Snap router
  Just model <- getParamT "model"
  Just field <- getParamT "field"
  (obj, failedTargets, succTargets, dupe) <-
      uploadInManyFields
             (\fName -> map (\i -> (model, i, field)) (readIds fName))
             (Just cutIds)
  writeLBS $ A.encode $ A.object [ "attachment" A..= obj
                                 , "targets"    A..= succTargets
                                 , "unknown"    A..= failedTargets
                                 , "dupe"       A..= dupe
                                 ]
      where
        -- Read a list of decimal instance ids from a file name,
        -- skipping everything else.
        readIds :: FilePath -> [Int]
        readIds fn =
            either (const []) id $
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
  -- 'Just' here, for these have already been matched by Snap router
  Just model <- getParamT "model"
  Just objId <- getIntParam "id"
  Just field <- getParamT "field"
  (res, fails, _, _) <-
    uploadInManyFields (const [(model, objId, field)]) Nothing
  if null fails
  then writeLBS $ A.encode $ res
  else error $ "Failed to upload in field: " ++ (show fails)


-- | Return path to an attached file (prepended by finished uploads
-- dir).
getAttachmentPath :: IdentI Attachment
                  -- ^ Attachment ID.
                  -> Handler b (FileUpload b) FilePath
getAttachmentPath aid = do
  obj <- either (error $ "No attachment " ++ show aid) id <$>
         (withLens pg $ liftPG' $ DB.read aid)
  fPath <- gets finished
  let fName = obj `Patch.get'` Attachment.filename
  return $
    fPath </> "attachment" </>
    (T.unpack $ identToRawFieldValue aid) </> T.unpack fName


-- | Append a reference of form @attachment:213@ to a field of another
-- instance, which must exist. This handler is thread-safe.
attachToField :: forall m b. (Model m, Typeable m) =>
                 IdentI m
              -- ^ Target object.
              -> Text
              -- ^ Field name in target instance.
              -> Text
              -- ^ A reference to an attachment instance to be added
              -- in a field of target instance.
              -> Handler b (FileUpload b) ()
attachToField instanceId field ref = do
  l <- gets locks
  -- Lock the field or wait for lock release
  liftIO $ atomically $ do
    hs <- readTVar l
    if HS.member lockName hs
    then retry
    else writeTVar l (HS.insert lockName hs)

  -- NB: If an error happens, the lock is never released
  let fName = someSymbolVal $ T.unpack field
  case fName of
    SomeSymbol (Proxy :: Proxy n) ->
      let
        -- Build field accessor on the fly
        acc = const Field :: m -> F (Maybe Reference) n ""
      in do
        -- Append new ref to the target field
        oldRefs <- (`Patch.get'` acc) <$>
                   (either (error $ "No object " ++ show instanceId) id <$>
                    (withLens pg $ liftPG' $ DB.read instanceId))
        let newRefs = addRef oldRefs ref
        void $ withLens pg $ liftPG' $
          DB.update instanceId (Patch.put acc newRefs $ Patch.empty)
  -- Unlock the field
  liftIO $ atomically $ do
    hs <- readTVar l
    writeTVar l (HS.delete lockName hs)
  return ()
    where
      addRef Nothing                r = Just $ Reference r
      addRef (Just (Reference ""))  r = Just $ Reference r
      addRef (Just (Reference val)) r =
        Just $ Reference $ T.concat [val, ",", r]
      lockName = T.concat [ modelName (modelInfo :: ModelInfo m)
                          , ":"
                          , identToRawFieldValue instanceId
                          , "/"
                          , field
                          ]


-- | Error which occured when processing an uploaded part.
type PartError = (PartInfo, PolicyViolationException)


-- | Process all files in the request and collect results. Files are
-- deleted after the handler runs. To permanently store the uploaded
-- files, copy them in the handler.
withUploads :: (PartInfo -> FilePath -> IO a)
            -- ^ Handler for successfully uploaded files.
            -> Handler b (FileUpload b) [Either PartError a]
withUploads proceed = do
  tmpDir <- gets tmp
  cfg <- gets cfg
  fns <- handleFileUploads tmpDir cfg (const $ partPol cfg) $
    curry (\(info, r) -> case r of
      Right tmp -> Right <$> proceed info tmp
      Left e    -> return $ Left (info, e)
      )
  return fns


-- | Helper which extracts first non-erroneous element from
-- 'withUploads' result or raises error if there's no such element.
oneUpload :: [Either PartError a] -> Handler b (FileUpload b) a
oneUpload res =
    case partitionEithers res of
      (_,         (f:_)) -> return f
      (((_, e):_), _   ) -> error $ T.unpack $ policyViolationExceptionReason e
      ([], [])           -> error "No uploaded parts provided"


-- | Store files from the request, return full paths to the uploaded
-- files. Original file names are preserved.
doUpload :: FilePath
         -- ^ Store files in this directory (relative to finished
         -- uploads path)
         -> Handler b (FileUpload b) [Either PartError FilePath]
doUpload relPath = do
  root <- gets finished
  let path = root </> relPath
  withUploads $ \info tmp ->
      do
        let justFname = T.unpack .T.decodeUtf8 . fromJust $ partFileName info
            newPath = path </> justFname
        createDirectoryIfMissing True path
        copyFile tmp newPath
        return newPath


-- | Store files from the request in the temporary dir, return pairs
-- @(original file name, path to file)@.
doUploadTmp :: Handler b (FileUpload b) [Either PartError (FilePath, FilePath)]
doUploadTmp = do
  tmpDir <- gets tmp
  withUploads $ \info tmp ->
      do
        let name = case partFileName info of
                     Just fn -> T.unpack $ T.decodeUtf8 fn
                     Nothing -> takeFileName tmp
        (newPath, _) <- openTempFile tmpDir name
        copyFile tmp newPath
        return (name, newPath)


partPol :: UploadPolicy -> PartUploadPolicy
partPol = allowWithMaximumSize . getMaximumFormInputSize


fileUploadInit :: SnapletLens b Postgres
               -> SnapletInit b (FileUpload b)
fileUploadInit pg =
    makeSnaplet "fileupload" "fileupload" Nothing $ do
      cfg      <- getSnapletUserConfig
      maxFile  <- liftIO $ lookupDefault 100  cfg "max-file-size"
      minRate  <- liftIO $ lookupDefault 1000 cfg "min-upload-rate"
      kickLag  <- liftIO $ lookupDefault 10   cfg "min-rate-kick-lag"
      inact    <- liftIO $ lookupDefault 20   cfg "inactivity-timeout"
      tmp      <- liftIO $
                  lookupDefault "/tmp"
                  cfg "tmp-path"
      finished <- liftIO $
                  lookupDefault "resources/static/fileupload"
                  cfg "finished-path"
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
      return $ FU pol tmp finished l pg
