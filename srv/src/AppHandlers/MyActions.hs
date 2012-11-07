
module AppHandlers.MyActions
  (myActionsHandler
  ) where


import Data.Map (Map)
import qualified Data.Map as Map

import Control.Concurrent.STM
----------------------------------------------------------------------
import Application
import CustomLogic.ActionAssignment



myActionsHandler :: AppHandler ()
myActionsHandler = do
  Just cUsr <- with auth currentUser
  let uLogin = userLogin cUsr
  logdUsers <- addToLoggedUsers cUsr

  actLock <- gets actionsLock
  do -- bracket_
    (liftIO $ atomically $ takeTMVar actLock)
    actions <- filter ((== Just "0") . Map.lookup "closed")
           <$> with db (DB.readAll "action")
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
    writeJSON $ myNewActions ++ myOldActions


writeJSON :: Aeson.ToJSON v => v -> AppHandler ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v
