{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Bulk import handlers. -}

module AppHandlers.Bulk
     ( vinImport
     , vinImportDirectory
     )

where

import           BasicPrelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Char (toLower)
import qualified Data.Configurator as Cfg
import           Data.Either (isLeft)
import qualified Data.Map as Map
import           Data.Pool (withResource)
import           Data.Text (pack, unpack)
import qualified Data.Vector as V

import           Control.Monad.State.Class

import           System.Directory
import           System.FilePath (pathSeparators, takeExtension, (</>))
import           System.IO

import           Database.Persist
import           Database.Persist.Sql (SqlBackend, fromSqlKey)


import           Carma.VIN
import           Snap

import           AppHandlers.Util
import           Application

import           Carma.Model.Program.Persistent
import qualified Carma.Model.Role as Role
import           Carma.Model.SubProgram.Persistent
import qualified Carma.Model.Usermeta as Usermeta
import           Carma.Model.VinFormat.Persistent
import           Data.Model (Ident (..))
import qualified Data.Model.Patch as Patch

import           Snap.Snaplet.Persistent
import           Snap.Snaplet.PostgresqlSimple
import           Snaplet.Auth.PGUsers
import           Snaplet.FileUpload (doUploadTmp, oneUpload, tmp)
import           Snaplet.TaskManager as TM
import           Util as U


-- Check user permissions
-- Allow users with partner role to upload files only to their
-- assigned subprograms.
checkUserPermissions :: Maybe Int -> Handler App App Int
checkUserPermissions subProgramId = do
  Just user <- currentUserMeta
  let Just (Ident uid) = Patch.get user Usermeta.ident
  let Just roles       = Patch.get user Usermeta.roles
  let Just subPrograms = Patch.get user Usermeta.subPrograms

  let validSubProgram  = case subProgramId of
                           Just sid -> V.elem (Ident sid) subPrograms
                           Nothing  -> True

  unless ((V.elem Role.partner roles && validSubProgram) ||
           V.elem Role.vinAdmin roles ||
           V.elem Role.psaanalyst roles) $
         handleError 403

  return uid

-- | Read @program@/@subprogram@/@format@ parameters and upload a VIN
-- file.
--
-- TODO Allow to select only program w/o subprogram.
--
-- TODO Use carma-models API to select user programs.
--
-- TODO Should VIN files really be stored permanently?
vinImport :: AppHandler ()
vinImport = logExceptions "Bulk/vinImport" $ do
  subprog <- getParam "subprogram"
  format <- getParam "format"

  case (B.readInt =<< subprog, B.readInt =<< format) of
    (Just (sid, _), Just (fid, _)) -> do
      syslogJSON Info "Bulk/vinImport" ["subprogram" .= sid, "format" .= fid]

      uid <- checkUserPermissions $ Just sid

      (inName, inPath) <- with fileUpload $ oneUpload =<< doUploadTmp

      syslogJSON Info "Bulk/vinImport" ["file" .= inName]
      tmpDir <- with fileUpload $ gets tmp
      (outPath, _) <- liftIO $ openTempFile tmpDir inName

      pgs <- with db getPostgresState

      -- VIN import task handler
      with taskMgr $ TM.create $ do
        let opts = Options inPath outPath
                   uid -- Set current user as committer
                   fid Nothing (Just sid) False
            connGetter = case pgs of
                           PostgresPool pool -> withResource pool
                           PostgresConn c    -> ($ c)
        res <- connGetter (doImport opts)

        removeFile inPath
        case res of
          Right (ImportResult (total, good, bad)) ->
              if bad == 0
              then removeFile outPath >>
                   return (Right (Aeson.String $ pack . show $ good, []))
              else return $ Right (Aeson.toJSON stats, [outPath])
                where
                  stats :: Map String Int64
                  stats =  Map.fromList [ ("good", good)
                                        , ("bad", bad)
                                        , ("total", total)
                                        ]
          Left e ->
              do
                ex <- doesFileExist outPath
                when ex $ removeFile outPath
                return $ Left $ Aeson.toJSON e
    _ -> do
      let err = "Subprogram/format not specified"
      syslogJSON Warning "Bulk/vinImport" ["err" .= err]
      error err


contractExtension, inDirectory, errorDirectory :: String
contractExtension = ".csv"
inDirectory = "IN"
errorDirectory = "ERR"

-- | Import VIN files from shared directory.
-- | Format of directory name is "program - subprogram/format".
-- | Directory hierarhy will be created after first call and
-- | restored after each call
vinImportDirectory :: AppHandler ()
vinImportDirectory = do
  cfg <- getSnapletUserConfig
  (importDir :: FilePath) <- liftIO $
    Cfg.require cfg "contracts.importing.from-directory.import-dir"
  (lockFile :: FilePath) <- liftIO $ (importDir </>) <$>
    Cfg.require cfg "contracts.importing.from-directory.lock-file"
  (extension :: String) <- liftIO $ toLowerCase <$>
    Cfg.lookupDefault contractExtension cfg
      "contracts.importing.from-directory.extension"

  -- If directory doesn't exists it is defenitely not mounted.
  -- Creating new empty directory to mount remote one later,
  -- also creating lock file there to prevent it from touching by default.
  liftIO $ (\m -> doesDirectoryExist importDir >>= flip unless m) $ do
    createDirectoryIfMissing True importDir
    System.IO.writeFile lockFile mempty

  -- Blocking access to plug directory, it is probably not mounted yet
  -- or connection is lost.
  (\m -> liftIO (doesFileExist lockFile) >>= flip when m)
    $  fail
    $  "Lock-file blocked importing from directory "
    ++ "(remote directory is not mounted?)"

  pgs <- with db getPostgresState

  (programs, subPrograms, vinFormats) <- with db2 $ runPersist $ do
    (programEntities :: [Entity Program]) <- selectList [] []
    let programs = for programEntities $ \e ->
                     (cleanPath $ programLabel $ entityVal e, entityKey e)

    (subProgramEntities :: [Entity SubProgram]) <- selectList [] []
    let subPrograms = for subProgramEntities $ \e ->
                      let val = entityVal e
                      in ( cleanPath $ subProgramLabel val
                         , entityKey e
                         , subProgramParent val
                         )

    (vinFormatEntities :: [Entity VinFormat]) <- selectList [] []
    let vinFormats = for vinFormatEntities $ \e ->
                       ( cleanPath $ vinFormatLabel $ entityVal e
                       , entityKey e)

    pure (programs, subPrograms, vinFormats)

  uid <- checkUserPermissions Nothing

  with taskMgr $ TM.create $ do
    let paths = concat $
                for programs $ \(programLabel, programId) ->
                  let programDir = toLowerCase programLabel
                      -- select subPrograms for program by programId
                      subPrograms' = filter (\(_, _, parent) ->
                                               programId == parent
                                            )
                                     subPrograms
                  in concat $
                     for subPrograms' $ \(subProgramLabel, subProgramId, _) ->
                       for vinFormats $ \(vinFormatLabel, vinFormatId) ->
                         let subProgramDir = programDir ++ " - " ++
                                             subProgramLabel ++
                                             "." ++ showSqlKey subProgramId
                             vinFormatDir = vinFormatLabel ++ "." ++
                                            showSqlKey vinFormatId
                         in ( toLowerCase (importDir </>
                                           subProgramDir </>
                                           vinFormatDir)
                            , (fromSqlKey subProgramId, fromSqlKey vinFormatId)
                            )


    files <- liftIO $
                fmap concat $
                forM paths $ \(d, (subProgramId, vinFormatId)) -> do
                  let inDir = d </> inDirectory
                  let errDir = d </> errorDirectory
                  createDirectoryIfMissing True inDir
                  createDirectoryIfMissing True errDir

                  fmap (map (\f -> ( inDir </> f
                                  , errDir </> f
                                  , fromIntegral subProgramId
                                  , fromIntegral vinFormatId
                                  )
                            ) .
                        filter ((extension ==) . takeLowerExtension)
                       ) $
                     listDirectory inDir

    let connGetter = case pgs of
                       PostgresPool pool -> withResource pool
                       PostgresConn c    -> ($ c)

    (messages, filenames) <-
      fmap ((unzip . map (\(Left message, filename) -> (message, filename)))
            . filter (isLeft . fst)) $
      forM files $ \(inPath, outPath, subProgramId, vinFormatId) -> do
        let options = Options inPath outPath uid vinFormatId
                              Nothing (Just subProgramId) False
        res <- connGetter (doImport options)

        case res of
          Right (ImportResult (total, good, bad)) ->
            if bad == 0
            then removeFile inPath >>
                 return (Right $ Aeson.String $ pack . show $ good, [])
            else return (Left $ Aeson.toJSON stats, outPath)
              where stats = Map.fromList [ ("good",  good)
                                         , ("bad",   bad)
                                         , ("total", total)
                                         ] :: Map String Int64

          Left e -> do
            exist <- doesFileExist outPath
            when exist $ removeFile outPath
            return (Left $ Aeson.toJSON e, inPath)

    return $ Right (Aeson.toJSON messages, filenames)

    where cleanPath = filter (`notElem` invalidChars) . unpack
          invalidChars = '"' : pathSeparators ++ "«»"
          toLowerCase = map toLower
          takeLowerExtension = toLowerCase . takeExtension
          for = flip map

          showSqlKey :: ToBackendKey SqlBackend record => Key record -> String
          showSqlKey = show . fromSqlKey
