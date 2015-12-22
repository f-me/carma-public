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
import           Data.Configurator
import qualified Data.Map as Map
import qualified Data.Vector as V

import           System.Directory
import           System.IO

import           Snap
import           Carma.VIN

import           Application
import           AppHandlers.Util
import           Database.PostgreSQL.Simple as Pg

import           Data.Model (Ident(..))
import qualified Data.Model.Patch as Patch
import qualified Carma.Model.Role as Role
import qualified Carma.Model.Usermeta as Usermeta

import           Snaplet.Auth.PGUsers
import           Snaplet.FileUpload (tmp, doUploadTmp, oneUpload)
import           Snaplet.TaskManager as TM
import           Util as U hiding (render)


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

      when (not
            $  (V.elem Role.partner roles && V.elem (Ident sid) subPrograms)
            || (V.elem Role.vinAdmin roles)
            || (V.elem Role.psaanalyst roles)) $
            handleError 403

      (inName, inPath) <- with fileUpload $ oneUpload =<< doUploadTmp

      syslogJSON Info "Bulk/vinImport" ["file" .= inName]
      tmpDir <- with fileUpload $ gets tmp
      (outPath, _) <- liftIO $ openTempFile tmpDir inName

      -- Use connection information from DbLayer
      appCfg <- getSnapletUserConfig
      connInfo <- liftIO $ ConnectInfo
                  <$> require appCfg "vinnie_host"
                  <*> require appCfg "vinnie_port"
                  <*> require appCfg "vinnie_user"
                  <*> require appCfg "vinnie_pass"
                  <*> require appCfg "vinnie_db"


      -- VIN import task handler
      with taskMgr $ TM.create $ do
        let opts = Options connInfo inPath outPath
                   uid -- Set current user as committer
                   fid Nothing (Just sid) False
        res <- doImport opts

        removeFile inPath
        case res of
          Right (ImportResult (total, good, bad)) ->
              if bad == 0
              then removeFile outPath >>
                   (return $ Right (Aeson.String $ show good, []))
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
