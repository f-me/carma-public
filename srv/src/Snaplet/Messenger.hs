{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Snaplet.Messenger ( Messenger
                         , newMessenger
                         , messengerInit
                         , sendMessage
                         , sendMessageIO
                         , Topic) where

import           BasicPrelude

import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.STM
import           Control.Exception (finally)
import           Control.Monad.State.Class
import           Data.Aeson as Aeson
import qualified Data.Set as Set
import           Network.WebSockets as WS
import           Network.WebSockets.Snap

import           Snap

-- FIXME: Add logger, find a way how tu run SimpleLog insine IO
-- import           Snap.Snaplet.SimpleLog


type Topic = Text


-- | A message from a client to handle subcriptions
data Subscription = Subscribe Topic | UnSubscribe Topic deriving Show


instance FromJSON Subscription where
  parseJSON (Object o) =  (Subscribe   <$> o .: "subscribe")
                      <|> (UnSubscribe <$> o .: "unsubscribe")
  parseJSON _ = mzero


instance ToJSON   Subscription where
  toJSON (Subscribe   topic) = object ["subscribe"   .= topic ]
  toJSON (UnSubscribe topic) = object ["unsubscribe" .= topic ]


instance WebSocketsData (Maybe Subscription) where
  fromDataMessage = \case
    WS.Text bs _ -> Aeson.decode bs
    WS.Binary bs -> Aeson.decode bs
  fromLazyByteString = Aeson.decode
  toLazyByteString   = Aeson.encode


-- | A message for clients
data Payload a = Payload Topic a deriving Show


instance  FromJSON a => FromJSON (Payload a) where
  parseJSON (Object o) =
    Payload     <$> o .: "topic" <*> (parseJSON =<< o .: "payload")
  parseJSON _ = mzero


instance ToJSON a => ToJSON (Payload a) where
  toJSON (Payload topic payload) =
    object [ "topic"   .= topic , "payload" .= payload ]


instance (ToJSON a, FromJSON a) => WebSocketsData (Payload a) where
  fromDataMessage
    = fromMaybe (error "fromDataMessage: could not parse Payload")
    . Aeson.decode
    . (\case WS.Text bs _ -> bs ; WS.Binary bs -> bs)
  fromLazyByteString
    = fromMaybe (error "fromLazyByteString: could not parse Payload")
    . Aeson.decode
  toLazyByteString = Aeson.encode


data Messenger = Messenger
    { queue :: TChan Msg
    }


data Msg = forall a. (FromJSON a, ToJSON a) => Msg (Payload a)


type MessengerHandler b t = Handler b Messenger t


-- | Subscriptions and connections handler
handler :: TChan Msg -> PendingConnection -> IO ()
handler queue pending = do
  conn <- acceptRequest pending
  subs <- newTVarIO Set.empty
  qChan <- atomically $ dupTChan queue
  qThread <- forkIO $ forever $ join $ atomically $ do
    Msg pl@(Payload topic _) <- readTChan qChan
    subs' <- readTVar subs
    return $ when (Set.member topic subs') (sendTextData conn pl)
  void $ flip finally (disconnect conn qThread) $ loop conn subs
  where
    loop conn subs = do
      req <- receiveData conn
      case req of
        Just req' ->
          atomically $ modifyTVar' subs $
          case req' of
            Subscribe topic -> Set.insert topic
            UnSubscribe topic -> Set.delete topic
        -- FIXME: need better handling of bad messages
        _ ->
          sendClose conn
          ("Unknown message, expecting subscription request" :: Text)
      loop conn subs

    disconnect c t = killThread t >> sendClose c ("bye" :: Text)


newMessenger :: IO Messenger
newMessenger = Messenger <$> newBroadcastTChanIO

messengerInit :: Messenger -> SnapletInit b Messenger
messengerInit msg@(Messenger q) =
  makeSnaplet "messages" "Simple WebSocket messaging service" Nothing $ do
    addRoutes [("/", runWebSocketsSnap (handler q))]
    -- FIXME: Restore this when will be found way to use logger inside IO
    -- timer with subscriptions logger
    -- void $ liftIO $ forkIO $ void $ repeatedTimer (showSubs m) (sDelay 1)
    return msg
  -- where
  --   showSubs m = do
  --     s <- readIORef m
  --     mapM_ (\(k, v) -> print $ (unpack k) ++ ": " ++ (show $ Set.size v))
  --       $ Map.toList s


-- Public API ------------------------------------------------------------------

-- | Send a message to subscribers of a topic.
--
-- The message is written to the broadcast channel, so blocking is
-- minimal. When this returns, there's no guarantee that all clients
-- have actually received the message.
sendMessage :: (FromJSON a, ToJSON a) =>
               Topic -> a -> MessengerHandler b ()
sendMessage topic msg = do
  q <- gets queue
  liftIO $ atomically $ writeTChan q (Msg $ Payload topic msg)

sendMessageIO :: (FromJSON a, ToJSON a) =>
               Messenger -> Topic -> a -> IO ()
sendMessageIO (Messenger q) topic msg =
  atomically $ writeTChan q (Msg $ Payload topic msg)
