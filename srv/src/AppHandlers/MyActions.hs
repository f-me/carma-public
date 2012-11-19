
module AppHandlers.MyActions
  (myActionsHandler
  ) where

import Control.Monad
import Data.Functor
import Data.String (fromString)

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T
import Data.Aeson as Aeson

import Control.Concurrent.STM
import Data.Time

import Snap
import Snap.Snaplet
import Snap.Snaplet.Auth
import Database.PostgreSQL.Simple
----------------------------------------------------------------------
import Application
import AppHandlers.Util
import qualified Snaplet.DbLayer as DB
import CustomLogic.ActionAssignment


selectActions :: AppHandler [Map ByteString ByteString]
selectActions = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString $
    "select id::text, assignedTo, "
    ++ "(extract (epoch from dueTime)::int)::text, "
    ++ "garbage::hstore -> 'priority', garbage::hstore -> 'targetGroup' "
    ++ "from actiontbl where "
    ++ "extract (epoch from dueTime) > 0 "
    ++ "and closed = false"
  let fields = ["id", "assignedTo", "duetime", "priority", "targetGroup"]
  return $ map (Map.fromList . zip fields . map (fromMaybe "")) rows


myActionsHandler :: AppHandler ()
myActionsHandler = do
  Just cUsr <- with auth currentUser
  let uLogin = userLogin cUsr
  logdUsers <- addToLoggedUsers cUsr

  actLock <- gets actionsLock
  do -- bracket_
    (liftIO $ atomically $ takeTMVar actLock)
    actions <- selectActions
    now <- liftIO getCurrentTime
    let (newActions,oldActions) = assignActions now actions (Map.map snd logdUsers)
    let myNewActions = take 5 $ Map.findWithDefault [] uLogin newActions
    with db $ forM_ myNewActions $ \act ->
      case Map.lookup "id" act of
        Nothing -> return ()
        Just actId -> void $ DB.update "action"
          (last $ B.split ':' actId)
          $ Map.singleton "assignedTo" $ T.encodeUtf8 uLogin
    let myOldActions = Map.findWithDefault [] uLogin oldActions
    (liftIO $ atomically $ putTMVar actLock ())
    let myActions = map (Map.! "id") $ myNewActions ++ myOldActions
    (with db $ mapM (DB.read "action") myActions) >>= writeJSON

