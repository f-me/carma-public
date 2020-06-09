{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

Thread-safe task manager for time-consuming operations which produce a
result and files.

Task result/error messages are encoded using JSON. Result files can be
accessed using a special handler.

Task creation handler is not installed under any common route. The
reason for that is due to different options and contexts required by
tasks (which otherwise would have to be brought under TaskManager
types).

Example:

Assume that TaskManager snaplet is installed under @/tasks@ path and a
another handler creates a new task using 'create':

> /createMyTask/?opts=foo

In response, task token is served in JSON:

> {"token":"a7c3db2"}

Now its status can be queried:

> /tasks/a7c3db2/status
> {"status":"inprogress"}

> /tasks/a7c3db2/status
> {"status":"failed", "msg":<JSON produced by task handler>}

> /tasks/a7c3db2/status
> {"status":"finished", "msg":<JSON>, "files":["foo.txt", "bar.pdf"]}

TaskManager does not specify how result/error JSON should be formed.
The client is expected to be able to handle JSON in @msg@ field,
knowing how the task was created.

Result files can be accessed using the provided token and a file name:

> /tasks/a7c3db2/files/foo.txt
> /tasks/a7c3db2/files/bar.pdf

A task and its files may be removed by sending a DELETE request:

> /tasks/a7c3db2

-}

module Snaplet.TaskManager
    ( create
    , TaskWorker
    , TaskError
    , TaskResult
    , TaskManager
    , taskManagerInit
    )

where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Control.Monad.State

import Data.Aeson as A hiding (Result)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString (ByteString)

import Data.Digest.Pure.SHA (sha256)

import Data.HashSet as HS hiding (map)
import Data.List as L
import Data.Map as M hiding (map)

import Data.Text hiding (map)
import Data.Text.Encoding

import System.Directory
import System.FilePath
import System.Random

import Snap.Core
import Snap.Snaplet
import Snap.Util.FileServe
import Carma.Utils.Snap (writeJSON)


-- | An action which may either fail or produce a result.
type TaskWorker = IO (Either TaskError TaskResult)


-- | Successfully finished tasks produce a message and a list of
-- output files.
type TaskResult = (A.Value, [FilePath])


type TaskError = A.Value


data TaskState = InProgress
               | Finished TaskResult
               | Failed TaskError

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


-- | Unique token used to access task information.
type Token = ByteString


data TaskManager a = TaskManager
    { tasks       :: TVar (M.Map Token Task)
    , rng         :: TVar StdGen
    -- ^ RNG for tokens.
    , locks       :: TVar (HS.HashSet Token)
    }


routes :: [(ByteString, Handler b (TaskManager b) ())]
routes = [ ("/:token/status",      method GET status)
         , ("/:token/files/:file", method GET getFile)
         , ("/:token",             method DELETE cleanup)
         ]


-- | Create a new task, serve JSON with its token.
create :: TaskWorker
       -> Handler b (TaskManager b) ()
create handler = do
  -- Generate a new token
  r <- gets rng
  tokenStr <- liftIO $ atomically $ do
             gen <- readTVar r
             let (i, g) = next gen
             writeTVar r g
             return $ show $ sha256 $ BL8.pack $ show i
  let token = B8.pack tokenStr

  -- Start a new task
  ts' <- gets tasks
  liftIO $ atomically $
         modifyTVar' ts' (M.insert token (Task InProgress))

  _ <- liftIO $ forkFinally handler $ \res ->
         atomically $ modifyTVar' ts' $ M.insert token $
           case res of
             Left e ->
                 Task $ Failed $ A.String $ pack $ show e
             Right (Left e) ->
                 Task $ Failed e
             Right (Right (msg, outPath)) ->
                 Task $ Finished (msg, outPath)

  -- Serve token to client
  writeJSON $ M.singleton ("token" :: String) tokenStr


-- | Serve current task status in JSON.
status :: Handler b (TaskManager b) ()
status = do
  Just token <- getParam "token"

  ts <- gets tasks >>= (liftIO . readTVarIO)
  case M.lookup token ts of
    Just s  -> writeJSON s
    Nothing -> error "Unknown token"


-- | Serve a file produced by a completed task. @token@ and @file@
-- parameters are read from the request, @file@ contains the expected
-- file basename.
getFile :: Handler b (TaskManager b) ()
getFile = do
  Just token <- getParam "token"
  Just file  <- getParam "file"
  let fn :: Text
      fn = decodeUtf8 file

  -- Pick a file to serve (we cannot serve the file from inside STM)
  ts <- gets tasks >>= (liftIO . readTVarIO)
  res <- case M.lookup token ts of
    Just (Task (Finished (_, []))) -> return $ Left "Task produced no files"
    Just (Task (Finished (_, files))) ->
      -- Find if any of output files match the requested name
      case L.find (\f -> pack (takeFileName f) == fn) files of
        Just f  -> return $ Right f
        Nothing -> return $ Left "Incorrect file name"
    Just (Task (Failed _)) -> return $ Left "Task produced no result"
    Just (Task InProgress) -> return $ Left "Task in progress"
    Nothing                -> return $ Left "Unknown token"

  case res of
    Left err -> error err
    Right f  -> do
      -- Lock the token until the file is served to ensure it's not
      -- accidentally cleaned up.
      lockToken token
      serveFile f
      releaseToken token


-- | Delete a finished task and its files.
cleanup :: Handler b (TaskManager b) ()
cleanup = do
  Just token <- getParam "token"

  -- Remove task entry and pick files to delete
  --
  -- Task entry is removed atomically, thus there's no need to protect
  -- files against double removal.
  tasks' <- gets tasks
  res <- liftIO $ atomically $ do
    ts <- readTVar tasks'
    case M.lookup token ts of
      Just (Task InProgress) -> return $ Left "Task in progress"
      Just (Task (Failed _)) -> do
        modifyTVar tasks' (M.delete token)
        return $ Right []
      Just (Task (Finished (_, files))) -> do
        modifyTVar tasks' (M.delete token)
        return $ Right files
      Nothing -> return $ Left "Unknown token"

  -- Remove task files
  case res of
    Left err    -> error err
    Right files -> do
      -- Wait if the file is being served
      waitToken token
      liftIO $ forM_ files removeFile


lockToken :: Token -> Handler b (TaskManager b) ()
lockToken token =
  waitTokenAnd (`modifyTVar'` HS.insert token) token


releaseToken :: Token -> Handler b (TaskManager b) ()
releaseToken token = do
  locks' <- gets locks
  liftIO $ atomically $ modifyTVar' locks' (HS.delete token)


waitToken :: Token -> Handler b (TaskManager b) ()
waitToken = waitTokenAnd (const $ return ())


-- | Retry as long as a token is locked, then proceed with an action.
waitTokenAnd :: (TVar (HS.HashSet Token) -> STM a)
             -> Token
             -> Handler b (TaskManager b) a
waitTokenAnd action token = do
  locks' <- gets locks
  liftIO $ atomically $ do
    hs <- readTVar locks'
    if HS.member token hs
    then retry
    else action locks'


-- | Create a new task manager snaplet.
taskManagerInit :: SnapletInit b (TaskManager b)
taskManagerInit = makeSnaplet "task-manager" "TaskMgr" Nothing $ do
   addRoutes routes
   TaskManager
     <$> liftIO (newTVarIO M.empty)
     <*> liftIO (newTVarIO =<< newStdGen)
     <*> liftIO (newTVarIO HS.empty)
