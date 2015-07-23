{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

AVAYA integration.

Interface with @dmcc-ws@ server via hooks and WebSocket connections.

- WebSocket proxy (@\/ws\/:ext@) binds used Avaya extensions to CaRMa
user id's;

- hooks allow DMCC to push information directly to CaRMa (such as
agent state changes and Avaya event history).

-}

module AppHandlers.Avaya
    ( dmccWsProxy
    , dmccHook

      -- * AVAYA state control
    , avayaToAfterCall
    , avayaToReady
    , setAgentState
    )

where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception (catches, handle, IOException)
import qualified Control.Exception as E (Handler(..))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor
import           Data.Maybe

import           Data.Aeson
import           Data.ByteString as BS
import           Data.CaseInsensitive (original)
import qualified Data.Map as Map
import           Data.Text as Text
import           Data.Time.Clock
import           Data.Vector (elem, fromList, singleton)

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ.Alt

import           Network.WebSockets
import           Network.WebSockets.Snap

import           Snap hiding (dir)
import           Snap.Snaplet.PostgresqlSimple hiding (query)


import           DMCC
import           DMCC.WebHook

import           Data.Model
import qualified Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch

import qualified Carma.Model.Action as Action
import           Carma.Model.AvayaEvent as AE
import           Carma.Model.AvayaEventType as AET
import           Carma.Model.Event as Event
import           Carma.Model.Role as Role (cti)
import           Carma.Model.Usermeta as Usermeta
import           Carma.Model.UserState as UserState

import           Application

import           AppHandlers.KPI (updateOperKPI)
import           AppHandlers.Util
import           Snaplet.Auth.Class
import           Snaplet.Auth.PGUsers
import           Snaplet.Messenger (sendMessage)
import           Snaplet.Messenger.Class (HasMsg, withMsg)
import           Util
import           Utils.Events
import           Utils.LegacyModel (mkIdentTopic)


-- | Check if a user has access to CTI.
isCtiUser :: Patch.Patch Usermeta -> Bool
isCtiUser um =
  Role.cti `Data.Vector.elem` roles
  where
    roles = fromMaybe (error "No roles in usermeta") $
            um `Patch.get` Usermeta.roles


-- | Proxy requests to/from dmcc-ws Web Socket, updating 'extMap'.
dmccWsProxy :: AppHandler ()
dmccWsProxy = do
  ext <- fromMaybe (error "No extension specified") <$> getIntParam "ext"
  eMap <- gets extMap
  um <- fromMaybe (error "No user") <$> currentUserMeta
  let reqMeta = fromMaybe (error "Bad meta")
      uid   = reqMeta $ um `Patch.get` Usermeta.ident
      uext  = reqMeta $ um `Patch.get` Usermeta.workPhoneSuffix
  when ((Text.pack $ show ext) /= uext) $
    error "Requested extension does not match that of the user"
  when (not $ isCtiUser um) $ error "No CTI access role"
  avayaConn <- liftIO newEmptyTMVarIO
  dmccWsHost' <- gets (dmccWsHost . options)
  dmccWsPort' <- gets (dmccWsPort . options)
  let dmccWsHost'' = Text.unpack $ fromMaybe "localhost" dmccWsHost'
      -- Client <-> CaRMa
      serverApp pending = do
        conn <- acceptRequest pending
        srvThread <- forkIO $ do
          avayaConn' <- liftIO $ atomically $ takeTMVar avayaConn
          handle (\(_ :: ConnectionException) ->
                    sendClose avayaConn' ("carma disconnected" :: ByteString)) $
            forever $ receive conn >>= send avayaConn'
        let killServer = killThread srvThread
        runClient dmccWsHost'' dmccWsPort' ("/" ++ show ext) (proxyApp conn)
          `catches`
          [ E.Handler $ \(_ :: ConnectionException) -> killServer
          , E.Handler $ \(_ :: IOException) -> killServer
          ]

      -- CaRMa <-> dmcc-ws
      proxyApp serverConn conn = do
        liftIO $ atomically $ do
          putTMVar avayaConn conn
          -- The last user to access an extension is its current user.
          -- We never remove users from this map (this would require
          -- re-implementing ref.checking/timeout code from dmcc-ws on
          -- CaRMa side and the expected memory footprint is small
          -- even for thousands of extensions)
          modifyTVar' eMap (Map.insert (Extension $ Text.pack $ show ext) uid)
        forever $ receive conn >>= send serverConn
  runWebSocketsSnap serverApp


-- | For every appropriate webhook call from dmcc library, create new
-- AvayaEvent if the user mentioned in the webhook is busy with an
-- action.
dmccHook :: AppHandler ()
dmccHook = do
  rsb <- readRequestBody 4096
  eMap <- gets extMap
  let -- Handle hook data for an agent if it's present in the
      -- extension-uid mapping
      handleWith :: Extension
                 -> (IdentI Usermeta -> AppHandler ())
                 -> AppHandler ()
      handleWith ext hdl = do
        am <- liftIO $ readTVarIO eMap
        case Map.lookup ext am of
          Nothing -> error $ "Hook data from unknown agent " ++ show rsb
          Just uid -> hdl uid
  now <- liftIO getCurrentTime
  case decode rsb of
    Nothing -> error $ "Could not read hook data " ++ show rsb
    Just (WHEvent (AgentId (_, ext)) (StateChange sn)) ->
      handleWith ext $ \uid -> do
        rememberAvayaSnapshot uid sn
        case _state sn of
          -- Automatic NotReady state (caused by an unanswered call or
          -- operator login) has empty reason code
          (Just (Settable DMCC.NotReady), "") -> switchToNA uid
          (Just (Settable DMCC.Ready), _) -> switchToReady uid
          _ -> return ()
    Just (WHEvent (AgentId (_, ext)) (TelephonyEvent ev st)) ->
      handleWith ext $ \uid -> do
          rememberAvayaSnapshot uid st
          let
            -- Map hook data to AvayaEventType dictionary and other
            -- AvayaEvent fields, decide if we need to write anything
            -- at all
            aeData =
              case ev of
                DeliveredEvent c _ _ _ _ ->
                  Just (c, et)
                  where
                    et = case dir of
                           (DMCC.In _) -> AET.in
                           _           -> AET.out
                    dir = direction call
                    call = fromMaybe (error "DeliveredEvent with bad state") $
                           Map.lookup c (_calls st)
                OriginatedEvent c _ _ ->
                  Just (c, AET.out)
                HeldEvent c ->
                  Just (c, AET.hold)
                RetrievedEvent c ->
                  Just (c, AET.unhold)
                ConferencedEvent _ s ->
                  Just (s, AET.conf)
                TransferedEvent _ s ->
                  Just (s, AET.transfer)
                ConnectionClearedEvent c _ ->
                  Just (c, AET.hangup)
                -- Ignore all other events
                _ -> Nothing
            in
              case aeData of
                Nothing -> return ()
                Just (cid@(CallId cidt), et) ->
                  let
                    (ucid', interlocs) =
                      case Map.lookup cid (_calls st) of
                        Just c ->
                          ((cidt `Text.append` "/" `Text.append`)
                            ((\(UCID u) -> u) $ DMCC.ucid c),
                           Prelude.map (\(DeviceId d) -> original d) $
                           DMCC.interlocutors c)
                        Nothing -> (cidt, [])
                  in do
                    (userState, model, actionId) <- userStateAction uid
                    when (userState == UserState.Busy &&
                          model == Data.Model.modelName
                          (modelInfo :: ModelInfo Action.Action)) $
                      void $ withAuthPg $ liftPG $ Patch.create $
                        Patch.put AE.ctime now $
                        Patch.put AE.eType et $
                        Patch.put AE.operator uid $
                        Patch.put AE.currentAction (Ident actionId) $
                        Patch.put AE.interlocutors (fromList interlocs) $
                        Patch.put AE.callId ucid'
                        Patch.empty
    -- Ignore errors (they shouldn't arrive via a webhook call
    -- anyways)
    Just (WHEvent _                  _) ->
      return ()


rememberAvayaSnapshot :: (HasMsg b, HasPostgresAuth b v) =>
                         IdentI Usermeta
                      -> DMCC.AgentSnapshot
                      -> Handler b v ()
rememberAvayaSnapshot uid sn = do
  let
    p = Patch.put
        Usermeta.lastAvayaSnapshot (fromMaybe Null $ decode $ encode sn)
        Patch.empty
  -- Update DB data
  void $ withAuthPg $ liftPG $ Patch.update uid p
  -- Update client data (via WebSocket)
  withMsg $ sendMessage (mkIdentTopic uid) p
  -- Update KPI screen data
  kpis <- withAuthPg $
          updateOperKPI (Data.Vector.singleton uid)
  withMsg $ sendMessage "oper-kpi" kpis



-- | Return last state and corresponding model name/id for a user.
userStateAction :: IdentI Usermeta
                -> AppHandler (UserStateVal, Text, Int)
userStateAction uid = do
  res <- withAuthPg $ liftPG $
    \c -> uncurry (query c)
    [sql|
     SELECT
     s.$(fieldPT UserState.state)$,
     e.$(fieldPT Event.modelName)$,
     e.$(fieldPT Event.modelId)$
     FROM
     $(tableQT UserState.ident)$ s,
     $(tableQT Event.ident)$ e
     WHERE
     e.$(fieldPT Event.ident)$ = s.$(fieldPT UserState.eventId)$
     AND s.$(fieldPT UserState.userId)$ = $(uid)$
     ORDER BY s.$(fieldPT UserState.ident)$
     DESC LIMIT 1
     |]
  case res of
    (h:_) -> return h
    _     -> error $ "No state for user " ++ show uid


-- | Switch current user to AfterCall agent state.
avayaToAfterCall :: AppHandler()
avayaToAfterCall =
  (fromMaybe (error "No user") <$> currentUserMeta) >>=
  setAgentState DMCC.AfterCall


-- | Switch current user to Ready agent state.
avayaToReady :: AppHandler()
avayaToReady =
  (fromMaybe (error "No user") <$> currentUserMeta) >>=
  setAgentState DMCC.Ready


-- | Send an agent state change request for a user, if
-- CTI is enabled for her.
setAgentState :: SettableAgentState
              -> Patch.Patch Usermeta
              -> AppHandler ()
setAgentState as um = do
  when (isCtiUser um) $ do
    dmccWsHost' <- gets (dmccWsHost . options)
    dmccWsPort' <- gets (dmccWsPort . options)

    let dmccWsHost'' = Text.unpack $ fromMaybe "localhost" dmccWsHost'
        ext = fromMaybe (error "Bad meta") $
              um `Patch.get` Usermeta.workPhoneSuffix
        miniApp conn =
          send conn (DataMessage $ Text $ encode (SetState as)) >>
          sendClose conn ("carma disconnected" :: ByteString) >>
          (void $ (receiveData conn :: IO ByteString))

    liftIO $ void $ forkIO $
      runClient dmccWsHost'' dmccWsPort' ("/" ++ Text.unpack ext) miniApp
