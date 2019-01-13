{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-| Bulk import handlers. -}

module AppHandlers.Bulk
    ( vinImport
    )

where

import           BasicPrelude

import           Control.Monad.State.Class
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import           Data.Pool (withResource)
import qualified Data.Vector as V
import           Data.Text (pack)

import           System.Directory
import           System.IO

import           Snap
import           Carma.VIN

import           Application
import           AppHandlers.Util

import           Data.Model (Ident(..))
import qualified Data.Model.Patch as Patch
import qualified Carma.Model.Role as Role
import qualified Carma.Model.Usermeta as Usermeta

import           Snaplet.Auth.PGUsers
import           Snaplet.FileUpload (tmp, doUploadTmp, oneUpload)
import           Snap.Snaplet.PostgresqlSimple
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
      let Just (Ident uid)  = Patch.get user Usermeta.ident
      let Just isUserActive = Patch.get user Usermeta.isActive
      let Just roles        = Patch.get user Usermeta.roles
      let Just subPrograms  = Patch.get user Usermeta.subPrograms

      unless isUserActive $ handleError 403

      unless (  (V.elem Role.partner roles && V.elem (Ident sid) subPrograms)
             ||  V.elem Role.vinAdmin roles
             ||  V.elem Role.psaanalyst roles
             ) $ handleError 403

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
