{-# LANGUAGE TemplateHaskell, FlexibleInstances, ExistentialQuantification #-}

module Snaplet.Messenger ( Messenger
                         , messengerInit
                         , sendMessage) where

import Prelude hiding (log)

import           Control.Applicative
import           Control.Monad
import           Control.Exception (finally)
import           Control.Lens (makeLenses)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.BoundedChan
import           Control.Concurrent.Suspend.Lifted
import           Control.Concurrent.Timer



import           Data.Maybe
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Text
import           Data.IORef
import           Data.Time.Clock.POSIX
import           Data.Aeson

import           Network.WebSockets
import           Network.WebSockets.Snap


import           Snap

-- FIXME: Add logger, find a way how tu run SimpleLog insine IO
-- import           Snap.Snaplet.SimpleLog




-- | Simple wrapper, so I can use define Ord and use Set for collection of
-- connections. 'time' is used as identifier of connections, because there is
-- instance of `Eq` and `Ord` for `Connection` and no easy way to implement
-- them.
data MConnection = MConnection POSIXTime Connection

instance Eq MConnection where
  (==) (MConnection t1 _) (MConnection t2 _) = t1 == t2

instance Ord MConnection where
  compare (MConnection t1 _) (MConnection t2 _) = compare t1 t2

-- | Subscriptions collection
type Subs = IORef (Map Text (Set MConnection))

-- | Wrapper so different payloads can be collcted in single queue
data Queued = forall a . WebSocketsData a => Queued Connection a

data Messenger = Messenger { _subscriptions :: Subs
                           , _queue         :: BoundedChan Queued
                           }
makeLenses ''Messenger

type MessengerHandler b t = Handler b Messenger t

-- | Message type to handle subcription state
data Subscription = Subscribe Text | UnSubscribe Text deriving Show

type Topic = Text
-- | Message type for sanding actual data
data Payload a = Payload Topic a deriving Show

instance FromJSON Subscription where
  parseJSON (Object o) =  (Subscribe   <$> o .: "subscribe")
                      <|> (UnSubscribe <$> o .: "unsubscribe")
  parseJSON _ = mzero

instance ToJSON   Subscription where
  toJSON (Subscribe   topic) = object ["subscribe"   .= topic ]
  toJSON (UnSubscribe topic) = object ["unsubscribe" .= topic ]

instance  FromJSON a => FromJSON (Payload a) where
  parseJSON (Object o) =
    (Payload     <$> o .: "topic" <*> (parseJSON =<< o .: "payload"))
  parseJSON _ = mzero

instance ToJSON a => ToJSON (Payload a) where
  toJSON (Payload topic payload) =
    object [ "topic"   .= topic , "payload" .= payload ]

instance (ToJSON a, FromJSON a) => WebSocketsData (Payload a) where
  -- FIXME: handle maybe value more gracefully
  fromLazyByteString = fromJust . decode
  toLazyByteString   = encode

instance (FromJSON a, ToJSON a) => WebSocketsData (Maybe a) where
  fromLazyByteString = decode
  toLazyByteString   = encode

-- | Subscriptions and connections handler
handler :: Subs -> PendingConnection -> IO ()
handler subs pending = do
  conn <- acceptRequest pending
  time <- getPOSIXTime
  let con' = MConnection time conn
  void $ flip finally (disconnect con' subs) $ loop con' subs
  where
    addSub conn = Map.alter (return . Set.insert conn . fromMaybe Set.empty)
    delSub conn = Map.adjust (Set.delete conn)

    loop conn@(MConnection _ c) s = do
      topic <- receiveData c
      let modifyRef f t = atomicModifyIORef' s ((,()) . f conn t)
      case topic of
        Just (Subscribe   topic') -> modifyRef addSub topic'
        Just (UnSubscribe topic') -> modifyRef delSub topic'
        -- FIXME: need better handling of bad messages
        _ -> sendClose c
             ("Unknown message, expecting subscription request" :: Text)
      loop conn s

    disconnect conn s = do
      atomicModifyIORef' s $ \m ->
        (Map.filter (not . Set.null) $ Map.map (Set.delete conn) m, ())

handleWS :: MessengerHandler b ()
handleWS = do
  m <- gets _subscriptions
  runWebSocketsSnap $ handler m

messengerInit :: SnapletInit b Messenger
messengerInit =
  makeSnaplet "messages" "Simple ws messaging service" Nothing $ do
    addRoutes [("/", handleWS)]
    m <- liftIO $ newIORef Map.empty
    q <- liftIO $ newBoundedChan 500
    -- worker thread that will actually send messages thru websocket
    void $ liftIO $ forkIO $ sendFromQ q
    -- timer with subscriptions logger
    -- void $ liftIO $ forkIO $ void $ repeatedTimer (showSubs m) (sDelay 1)
    return $ Messenger m q
  where
    showSubs m = do
      s <- readIORef m
      mapM_ (\(k, v) -> print $ (unpack k) ++ ": " ++ (show $ Set.size v))
        $ Map.toList s

-- | Really send messages from the queue. Use 'forkIO' for each message
-- so slow cliens wan't jam the queue.
sendFromQ :: BoundedChan Queued -> IO ()
sendFromQ q = do
  void $ readChan q >>= \(Queued c p) -> forkIO $ sendTextData c p
  sendFromQ q

-- | Try to put message in the queue, return false in case of fail (q is full
-- for example)
putInQ :: (FromJSON a, ToJSON a)
       => BoundedChan Queued -> Connection -> Payload a -> IO Bool
putInQ chan c p = liftIO $ tryWriteChan chan $ Queued c p

-- Public API ------------------------------------------------------------------

-- | Send payload to the topic subscribers.
-- This will not actually send message, just put it in the send queue
-- and return immediately and will never block.
sendMessage :: (ToJSON a, FromJSON a) => Text -> a -> MessengerHandler b ()
sendMessage topic msg = do
  subsRef <- gets _subscriptions
  q       <- gets _queue
  subs    <- liftIO $ readIORef subsRef
  case Map.lookup topic subs of
    Just cs -> void $ liftIO $ mapM_
      (\(MConnection _ c) -> putInQ q c (Payload topic msg)) $ Set.toList cs
    Nothing -> return ()
  return ()
