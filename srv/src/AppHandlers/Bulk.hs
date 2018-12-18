{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Bulk import handlers. -}

module AppHandlers.Bulk
    ( vinImport
    , vinImportDirectory
    )

where

import           BasicPrelude

import           Control.Monad.State.Class
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import           Data.Pool (withResource)
import qualified Data.Vector as V
import           Data.Text (pack, unpack)
import qualified Data.Configurator                 as Cfg
import           Data.Char                         (toLower)
import           Database.Persist
import           Database.Persist.Sql (SqlBackend, fromSqlKey)

import           System.Directory
import           System.IO
import           System.FilePath                   (pathSeparators,
                                                    takeExtension, (</>))


import           Snap
import           Carma.VIN

import           Application
import           AppHandlers.Util

import           Data.Model (Ident(..))
import qualified Data.Model.Patch as Patch
import qualified Carma.Model.Role as Role
import qualified Carma.Model.Usermeta as Usermeta
import           Carma.Model.Program.Persistent
import           Carma.Model.SubProgram.Persistent
import           Carma.Model.VinFormat.Persistent

import           Snaplet.Auth.PGUsers
import           Snaplet.FileUpload (tmp, doUploadTmp, oneUpload)
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Persistent
import           Snaplet.TaskManager as TM
import           Util as U


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

      -- Check user permissions
      -- Allow users with partner role to upload files only to their
      -- assigned subprograms.
      Just user <- currentUserMeta
      let Just (Ident uid) = Patch.get user Usermeta.ident
      let Just roles       = Patch.get user Usermeta.roles
      let Just subPrograms = Patch.get user Usermeta.subPrograms

      unless (
              (V.elem Role.partner roles && V.elem (Ident sid) subPrograms)
            || V.elem Role.vinAdmin roles
            || V.elem Role.psaanalyst roles) $
            handleError 403

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
                           PostgresConn c -> ($ c)
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

-- | Upload VIN files from shared directory.
-- | Format of directory name is "program - subprogram/format".
-- | Directory hierarhy will be created after first call and
-- | restored after each call
vinImportDirectory :: AppHandler ()
vinImportDirectory = do
  cfg <- getSnapletUserConfig
  importDir <- liftIO $
              Cfg.require cfg "contracts-import-dir"
                :: Handler App App String
  extension <- liftIO $ toLowerCase <$>
              Cfg.lookupDefault contractExtension cfg "contracts-extension"
                :: Handler App App String

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

  -- Check user permissions
  -- Allow users with partner role to upload files only to their
  -- assigned subprograms.
  Just user <- currentUserMeta
  let Just (Ident uid) = Patch.get user Usermeta.ident
  let Just roles       = Patch.get user Usermeta.roles

  unless ( V.elem Role.partner roles
         || V.elem Role.vinAdmin roles
         || V.elem Role.psaanalyst roles) $
         handleError 403

  with taskMgr $ TM.create $ do
    let paths = concat $
                for programs $ \(programLabel, programId) ->
                  let programDir = toLowerCase programLabel
                      -- select subPrograms for program by programId
                      subPrograms' = filter (\(_, _, parent) -> programId == parent)
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

                  pure =<< fmap (map (\f -> ( inDir </> f
                                           , errDir </> f
                                           , subProgramId
                                           , vinFormatId
                                           )
                                     ) .
                                 filter ((extension ==) . takeLowerExtension) ) $
                           listDirectory inDir

    let connGetter = case pgs of
                       PostgresPool pool -> withResource pool
                       PostgresConn c -> ($ c)

    contracts <- forM files $ \(inPath, outPath, subProgramId, vinFormatId) -> do
                  let options = Options inPath
                                        outPath
                                        uid
                                        (fromIntegral vinFormatId)
                                        Nothing
                                        (Just $ fromIntegral subProgramId)
                                        False
                  res <- connGetter (doImport options)

                  case res of
                    Right (ImportResult (total, good, bad)) ->
                      if bad == 0
                      then removeFile inPath >>
                           return (Right $ Aeson.String $ pack . show $ good, inPath)
                      else return $ (Right $ Aeson.toJSON stats, outPath)
                        where stats = Map.fromList [ ("good",  good)
                                                   , ("bad",   bad)
                                                   , ("total", total)
                                                   ] :: Map String Int64

                    Left e -> do
                      exist <- doesFileExist outPath
                      when exist $ removeFile outPath
                      return (Left $ Aeson.toJSON e, inPath)

    return $ Right (Aeson.toJSON contracts, [])

    where cleanPath = filter (`notElem` invalidChars) . unpack
          invalidChars = '"' : pathSeparators ++ "«»"
          toLowerCase = map toLower
          takeLowerExtension = toLowerCase . takeExtension
          for = flip map

          showSqlKey :: ToBackendKey SqlBackend record => Key record -> String
          showSqlKey = show . fromSqlKey
