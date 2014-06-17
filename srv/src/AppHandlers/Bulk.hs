{-# LANGUAGE DoAndIfThenElse #-}

{-| Bulk import handlers. -}

module AppHandlers.Bulk
    ( vinImport
    , partnerImport
    )

where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Configurator
import qualified Data.HashMap.Strict as HM
import           Data.Int
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Database.PostgreSQL.Simple (ConnectInfo(..))

import           System.Directory
import           System.FilePath
import           System.IO

import           Snap
import           Snap.Http.Server.Config as S
import           Snap.Snaplet.Auth hiding (session)
import           Snap.Util.FileServe (serveFileAs)

import qualified Carma.Model.Role as Role

import           Carma.Partner
import           Carma.VIN

import           Application
import           AppHandlers.Util

import           Snaplet.Auth.PGUsers
import qualified Snaplet.DbLayer.Types as DB
import           Snaplet.FileUpload (tmp, doUpload, doUploadTmp, oneUpload)
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
  prog <- getParam "program"
  subprog <- getParam "subprogram"
  format <- getParam "format"

  case (B.readInt =<< subprog, B.readInt =<< format) of
    (Just (sid, _), Just (fid, _)) -> do
      syslogJSON Info "Bulk/vinImport" ["subprogram" .= sid, "format" .= fid]

      -- Check user permissions
      Just u <- with auth currentUser
      u' <- with db $ replaceMetaRolesFromPG u
      -- Allow users with partner role to upload files only to their
      -- assigned subprograms. Note that usermeta field is still
      -- called "programs" despite storing a list of subprogram ids.
      let Aeson.String userSpgms' = HM.lookupDefault "" "programs" $ userMeta u'
          userSpgms = map fst $
                     mapMaybe B.readInt $
                     B.split ',' $ T.encodeUtf8 userSpgms'
      when (not $
            (elem (Role $ identFv Role.partner) (userRoles u') &&
             elem sid userSpgms) ||
            (elem (Role $ identFv Role.vinAdmin) (userRoles u')) ||
            (elem (Role $ identFv Role.psaanalyst) (userRoles u'))) $
            handleError 403

      (inName, inPath) <- with fileUpload $ oneUpload =<< doUploadTmp

      syslogJSON Info "Bulk/vinImport" ["file" .= inName]
      tmpDir <- with fileUpload $ gets tmp
      (outPath, _) <- liftIO $ openTempFile tmpDir inName

      -- Use connection information from DbLayer
      dbCfg <- with db $ with DB.postgres $ getSnapletUserConfig
      connInfo <-
          liftIO $ ConnectInfo
                     <$> require dbCfg "host"
                     <*> require dbCfg "port"
                     <*> require dbCfg "user"
                     <*> require dbCfg "pass"
                     <*> require dbCfg "db"

      -- Set current user as committer
      uid <- maybe (error "No usermeta id") fst <$> (with db $ userMetaPG u)

      -- VIN import task handler
      with taskMgr $ TM.create $ do
        let opts = Options connInfo inPath outPath uid fid Nothing (Just sid)
        res <- doImport opts

        removeFile inPath
        case res of
          Right (ImportResult (total, good, bad)) ->
              do
                if bad == 0
                then removeFile outPath >>
                     (return $ Right (Aeson.String $ T.pack $ show good, []))
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


-- | Upload a CSV file and update the partner database, serving a
-- report back to the client.
--
-- (carma-partner-import package interface).
--
-- TODO Use TaskManager
partnerImport :: AppHandler ()
partnerImport = logExceptions "Bulk/partnerImport" $ do
  sCfg <- liftIO $ commandLineConfig (emptyConfig :: S.Config Snap a)
  let carmaPort = case getPort sCfg of
                    Just n -> n
                    Nothing -> error "No port"
  tmpPath <- with fileUpload $ gets tmp
  (tmpName, _) <- liftIO $ openTempFile tmpPath "last-pimp.csv"

  syslogTxt Info "Bulk/partnerImport" "Uploading data"
  inPath <- with fileUpload $ oneUpload =<< doUpload "partner-upload-data"

  let outPath = tmpPath </> tmpName

  syslogJSON Info "Bulk/partnerImport" ["inPath" .= inPath, "outPath" .= outPath]

  syslogTxt Info "Bulk/partnerImport" "Loading dictionaries from CaRMa"
  Just dicts <- liftIO $ loadIntegrationDicts carmaPort
  syslogTxt Info "Bulk/partnerImport" "Processing data"
  liftIO $ processData carmaPort inPath outPath dicts
  syslogTxt Info "Bulk/partnerImport" "Serve processing report"
  serveFileAs "text/csv" outPath
