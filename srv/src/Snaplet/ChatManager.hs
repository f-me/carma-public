{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

{-|

WebSocket chat rooms for CaRMa users.

To connect to a room, open a WebSocket connection using
@/chat/:roomname@ address, where @chat@ is the top-level URL this
snaplet is installed under. Text datums sent via this connection are
chat room messages.

When a user first connects to a room, its participants receive an
announcement as JSON:

> {"joined":{"ip":"192.1.20.17","id":90}}

Upon each connection user recieves list of room participants:

> {"youAreNotAlone": [{"ip":"192.1.20.17","id":90}]}

User IP and usermeta ID also accompany messages sent by a user:

> {"msg":"hello there","user":{"ip":"192.1.20.17","id":90}}

Server messages sent via 'sendMessage' lack @user@ field in their
JSON.

When a user disconnects from a room a matching announcement is
published:

> {"left":{"ip":"192.1.20.17","id":90}}

Users with the same usermeta id from different IP addresses count as
different users when connects/disconnects are handled.

-}

module Snaplet.ChatManager
    ( ChatManager
    , chatInit
    , sendMessage
    , RoomName)

where

import           BasicPrelude

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.State.Class
import           Control.Lens (Lens')

import           Data.Aeson
import qualified Data.Map as Map

import           Network.WebSockets
import           Network.WebSockets.Snap

import           Data.Model

import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple (Postgres(..))

import           Snaplet.Auth.Class
import           Snaplet.Auth.PGUsers
import           Carma.Utils.Snap (withLens)


data ChatManager b = ChatManager
    { auth  :: Lens' b (Snaplet (AuthManager b))
    , db    :: Lens' b (Snaplet Postgres)
    , queue :: TChan (RoomName, ChatMessage)
    }


instance HasPostgresAuth b (ChatManager b) where
  withAuth = withLens auth
  withAuthPg = withLens db


type IpAddress = Text


-- | Multiple connections to the same room count as one.
newtype UserKey = UK (Maybe (IdentI Usermeta), IpAddress)
                  deriving (Eq, Ord, Show)

userKeyId :: UserKey -> Maybe (IdentI Usermeta)
userKeyId (UK (i, _)) = i


instance ToJSON UserKey where
  toJSON (UK (ident, ip)) = toJSON $ object ["id" .= ident, "ip" .= ip]


data ChatMessage = UserMessage (UserKey, Text)
                 | ServerMessage Text
                 | UserJoined UserKey
                 | UserLeft UserKey
                   deriving Show


type RoomName = Text


-- | Reference-counting tracker for chat room participants.
type ParticipantsMap =
  TMVar (Map.Map (UserKey, RoomName) Int)


-- | Connections handler
handler :: TChan (RoomName, ChatMessage)
        -> ParticipantsMap
        -> Handler b (ChatManager b) ()
handler queue refs = do
  rName <- fromMaybe (error "Empty chat room name") <$> getParam "room"
  addr <- rqClientAddr <$> getRequest
  uid <- currentUserMetaId
  let
    me = UK (uid, decodeUtf8 addr)
    ourRoom = decodeUtf8 rName
    ref = (me, ourRoom)
    serverApp pending = do
      conn <- acceptRequest pending
      -- Increment reference counter, announce us if we're a new
      -- participant in the room
      qChan <- atomically $ do
        r <- takeTMVar refs
        let oldCount = fromMaybe 0 $ Map.lookup ref r
        when (oldCount == 0) $ writeTChan queue (ourRoom, UserJoined me)
        putTMVar refs (Map.insert ref (oldCount + 1) r)
        dupTChan queue
      -- Send list of room participants to the user.
      -- (Filter out connected user if he is also in the room.)
      members <- atomically $ do
        xs <- Map.keys <$> readTMVar refs
        return
          [ user
          | (user, room) <- xs
          , room == ourRoom && userKeyId user /= uid]
      sendTextData conn $ encode $ object ["youAreNotAlone" .= members]
      -- Observe message queue channel and push messages in our room
      -- to the client
      qThread <- forkIO $ forever $ join $ atomically $ do
        (targetRoom, what) <- readTChan qChan
        return $ when (targetRoom == ourRoom) $
          case what of
            ServerMessage msg ->
              sendTextData conn $ encode $
              object ["msg" .= msg]
            UserMessage (who, msg) ->
              sendTextData conn $ encode $
              object ["msg" .= msg, "user" .= who]
            UserJoined who ->
              sendTextData conn $ encode $
              object ["joined" .= who]
            UserLeft who ->
              sendTextData conn $ encode $
              object ["left" .= who]
      let
        disconnectionHandler = do
          -- Decrement reference counter
          cnt <- do
            r <- atomically $ takeTMVar refs
            flip onException (atomically $ putTMVar refs r) $
              case Map.lookup ref r of
                Just cnt -> do
                  let newR = if cnt > 1
                             then Map.insert ref (cnt - 1) r
                             else Map.delete ref r
                  atomically $ putTMVar refs newR
                  return $ cnt - 1
                Nothing -> error $ "Releasing unknown agent " <> fromShow ref
          -- Announce if we're leaving
          when (cnt == 0) $ atomically $
            writeTChan queue (ourRoom, UserLeft me)
          killThread qThread
          sendClose conn ("Bye" :: Text)
      flip onException disconnectionHandler $ forever $ do
        msg <- decodeUtf8 <$> receiveData conn
        atomically $ writeTChan queue (ourRoom, UserMessage (me, msg))

  runWebSocketsSnap serverApp


-- | Send a server message to a room.
sendMessage :: RoomName
            -> Text
            -- ^ Message text.
            -> Handler b (ChatManager b) ()
sendMessage roomName msg = do
  q <- gets queue
  liftIO $ atomically $ writeTChan q (roomName, ServerMessage msg)


chatInit :: Lens' b (Snaplet (AuthManager b)) ->
            Lens' b (Snaplet Postgres) ->
            SnapletInit b (ChatManager b)
chatInit auth db =
  makeSnaplet "chat" "WebSocket chat rooms" Nothing $ do
    q <- liftIO newBroadcastTChanIO
    refs <- liftIO $ newTMVarIO Map.empty
    addRoutes [("/:room", handler q refs)]
    return $ ChatManager auth db q
