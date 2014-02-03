{-# LANGUAGE OverloadedStrings #-}

{-|

Task manager for time-consuming operations which require a file as
input and serve another file back.

Task creation handler is not installed under any common route. The
reason for that is due to different options and contexts required by
tasks (which otherwise would have to be brought under TaskManager
types).

-}

module Snaplet.TaskManager
    ( TaskManager
    , taskManagerInit
    , create
    , TaskHandler
    )

where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Control.Monad.State hiding (state)

import Data.Aeson as A hiding (Result)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString (ByteString)

import Data.Configurator
import Data.Digest.Pure.MD5 (md5)

import Data.Int
import Data.List as L
import Data.Map as M hiding (map)
import Data.Maybe as Maybe

import Data.Text hiding (head, map)
import Data.Text.Encoding

import System.Directory
import System.FilePath
import System.Random

import Snap.Core
import Snap.Snaplet
import Snap.Util.FileServe

import Snap.Extras.JSON

import Snaplet.FileUpload hiding (db)


type TaskHandler = IO (Either TaskError TaskResult)


-- | Successfully finished tasks produce a message and a list of
-- output files.
type TaskResult = (A.Value, [FilePath])


type TaskError = A.Value


data TaskState = InProgress
               | Finished TaskResult
               | Failed TaskError

-- Messages are double-JSON-encoded!
instance ToJSON Task where
    toJSON t = A.object $ case t of
        Task InProgress ->
            [ "status" .= A.String "inprogress"
            ]
        Task (Finished (msg, files)) ->
            [ "status" .= A.String "finished"
            , "msg"    .= msg
            , "files"  .= map (A.String . pack .  takeFileName) files ]
        Task (Failed msg) ->
            [ "status" .= A.String "failed"
            , "msg"    .= msg
            ]


newtype Task = Task TaskState


type Token = ByteString


data TaskManager a = TaskManager
    { tasks       :: TVar (M.Map Token Task)
    , rng         :: TVar StdGen
    -- ^ RNG for tokens.
    }


routes :: [(ByteString, Handler b (TaskManager b) ())]
routes = [ ("/status/:token",          method GET status)
         , ("/getFile/:token/:file",   method GET getFile)
         ]


maxOptionsSize :: Int64
maxOptionsSize = 32768


-- | Create a new task.
create :: TaskHandler
       -- ^ Task worker action.
       -> Handler b (TaskManager b) ()
create handler = do
  -- Generate new token
  r <- gets rng
  token <- liftIO $ atomically $ do
             gen <- readTVar r
             let (i, g) = next gen
             writeTVar r g
             return $ B8.pack $ show $ md5 $ BL8.pack $ show i

  -- Start a new task
  ts' <- gets tasks
  liftIO $ atomically $
         modifyTVar' ts' (M.insert token (Task InProgress))

  rb <- readRequestBody maxOptionsSize
  tid <- liftIO $ forkFinally handler $ \res ->
         atomically $ modifyTVar' ts' $ M.insert token $
           case res of
             Left e ->
                 (Task $ Failed $ A.String $ pack $ show e)
             Right (Left e) ->
                 (Task $ Failed e)
             Right (Right (msg, outPath)) ->
                 (Task $ Finished (msg, outPath))

  -- Serve token to client
  writeJSON $ M.singleton ("token" :: String) token


-- | Serve JSON with @status@ field.
status :: Handler b (TaskManager b) ()
status = do
  Just token <- getParam "token"

  ts <- gets tasks >>= (liftIO . readTVarIO)
  case M.lookup token ts of
    Just s  -> writeJSON s
    Nothing -> error "Unknown token"


-- | Serve a file of a completed task.
getFile :: Handler b (TaskManager b) ()
getFile = do
  Just token <- getParam "token"
  Just file  <- getParam "file"
  let fn :: Text
      fn = decodeUtf8 $ file

  ts <- gets tasks >>= (liftIO . readTVarIO)
  case M.lookup token ts of
    Just (Task (Finished (_, files))) ->
        -- Find if any of output files matche the requested name
        case L.find (\f -> (pack $ takeFileName f) == fn) files of
          Just f -> do
            -- Forget this task and serve the file to client.
            --
            -- TODO Dismiss tasks via another handler when a client
            -- has specifically requested to do so.
            ts' <- gets tasks
            liftIO $ atomically $ do
                writeTVar ts' =<< (M.delete token) <$> readTVar ts'
            serveFile f
          Nothing -> error "Incorrect file name"
    Just (Task _) -> error "Task produced no result"
    Nothing -> error "Unknown token"


taskManagerInit :: SnapletInit b (TaskManager b)
taskManagerInit = makeSnaplet "task-manager" "TaskMgr" Nothing $ do
   cfg <- getSnapletUserConfig
   addRoutes routes
   TaskManager
     <$> (liftIO $ newTVarIO M.empty)
     <*> (liftIO $ (newTVarIO =<< newStdGen))
