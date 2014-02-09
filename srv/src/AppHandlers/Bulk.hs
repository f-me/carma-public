{-| Bulk import handlers. -}

{-# LANGUAGE DoAndIfThenElse #-}

module AppHandlers.Bulk
    ( vinImport
    , partnerImport
    )

where

import Prelude hiding (log)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Configurator
import           Data.Int
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
import           Snap.Snaplet.SimpleLog
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
-- TODO Allow partners to upload VIN files only to their assigned
-- subprograms.
--
-- TODO Should VIN files really be stored permanently?
vinImport :: AppHandler ()
vinImport = scope "vin" $ scope "upload" $ do
  prog <- getParam "program"
  subprog <- getParam "subprogram"
  format <- getParam "format"

  case (B.readInt =<< subprog, B.readInt =<< format) of
    (Just (sid, _), Just (fid, _)) -> do
      log Trace $ T.concat ["Subprogram: ", T.pack $ show sid]
      log Trace $ T.concat ["Format: ", T.pack $ show fid]

      -- Check user permissions
      Just u <- with auth currentUser
      u' <- with db $ replaceMetaRolesFromPG u
      -- Allow users with partner role to upload files only to their
      -- assigned subprograms. Note that usermeta field is still
      -- called "programs" despite storing a list of subprogram ids.
--      let Aeson.String userPgms' = HM.lookupDefault "" "programs" $ userMeta u'
--          userPgms = B.split ',' $ T.encodeUtf8 userPgms'
      when (not $
--            (elem (Role $ identFv Role.partner) (userRoles u') && elem pgmId userPgms) ||
            (elem (Role $ identFv Role.vinAdmin) (userRoles u'))) $
            handleError 403

      (inName, inPath) <- with fileUpload $ oneUpload =<< doUploadTmp

      log Info $ T.concat ["Processing file: ", T.pack inName]
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
      --
      -- TODO Use usermeta id
      let Just (UserId uid') = userId u
          Just (uid, _)      = B.readInt $ T.encodeUtf8 uid'

      -- VIN import task handler
      with taskMgr $ TM.create $ do
        let opts = Options connInfo inPath outPath uid fid Nothing (Just sid)
        res <- doImport opts

        removeFile inPath
        case res of
          Right (good, bad) ->
              do
                if bad == 0
                then removeFile outPath >>
                     (return $ Right (Aeson.String $ T.pack $ show good, []))
                else return $ Right (Aeson.toJSON stats, [outPath])
                where
                  stats :: Map String Int64
                  stats =  Map.fromList [ ("good", good)
                                        , ("bad", bad)
                                        ]
          Left e ->
              do
                ex <- doesFileExist outPath
                when ex $ removeFile outPath
                return $ Left $ Aeson.toJSON e
    _ -> do
      log Error "Subprogram/format not specified"
      error "Subprogram/format not specified"


-- | Upload a CSV file and update the partner database, serving a
-- report back to the client.
--
-- (carma-partner-import package interface).
--
-- TODO Use TaskManager
partnerImport :: AppHandler ()
partnerImport = scope "partner" $ scope "upload" $ do
  sCfg <- liftIO $ commandLineConfig (emptyConfig :: S.Config Snap a)
  let carmaPort = case getPort sCfg of
                    Just n -> n
                    Nothing -> error "No port"
  tmpPath <- with fileUpload $ gets tmp
  (tmpName, _) <- liftIO $ openTempFile tmpPath "last-pimp.csv"

  log Trace "Uploading data"
  inPath <- with fileUpload $ oneUpload =<< doUpload "partner-upload-data"

  let outPath = tmpPath </> tmpName

  log Trace $ T.pack $ "Input file " ++ inPath
  log Trace $ T.pack $ "Output file " ++ outPath

  log Trace "Loading dictionaries from CaRMa"
  Just dicts <- liftIO $ loadIntegrationDicts carmaPort
  log Trace "Processing data"
  liftIO $ processData carmaPort inPath outPath dicts
  log Trace "Serve processing report"
  serveFileAs "text/csv" outPath
