{-# LANGUAGE OverloadedStrings #-}

{-|

Avaya snaplet provides interface with dmcc-ws via hooks and Web
Sockets.

- Web Socket proxy binds used Avaya extensions to CaRMa user id's;

- hooks allow DMCC to push information directly to CaRMa.

-}

module Snaplet.Avaya
    ( Avaya(..)
    , avayaInit
    )

where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception (finally)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor
import           Data.Maybe

import qualified Data.Map as Map
import           Data.Text as Text

import           Network.WebSockets
import           Network.WebSockets.Snap

import           Snap.Core
import           Snap.Snaplet

import           Data.Model
import           Carma.Model.Usermeta

import           AppHandlers.Util


data Avaya = Avaya
    { extMap :: TVar (Map.Map (IdentI Usermeta) Text)
    }


-- | Proxy requests to/from dmcc-ws Web Socket.
avayaWsProxy :: Maybe Text -> Int -> Handler a Avaya ()
avayaWsProxy dmccHost dmccPort = do
  ext <- fromMaybe (error "No extension specified") <$> getIntParam "ext"
  avayaConn <- liftIO $ newEmptyTMVarIO
  let dmccPort' = Text.unpack $ fromMaybe "localhost" dmccHost
      -- Client <-> CaRMa
      serverApp pending = do
        conn <- acceptRequest pending
        srvThread <- forkIO $ do
          avayaConn' <- liftIO $ atomically $ takeTMVar avayaConn
          forever $ receive conn >>= send avayaConn'
        (flip finally) (killThread srvThread) $
          runClient dmccPort' dmccPort ("/" ++ show ext) (proxyApp conn)
      -- CaRMa <-> dmcc-ws
      proxyApp serverConn conn = do
        liftIO $ atomically $ putTMVar avayaConn conn
        forever $ receive conn >>= send serverConn
  runWebSocketsSnap serverApp


hook :: Handler a Avaya ()
hook = error "Not implemented yet"


avayaInit :: Maybe Text -> Int -> SnapletInit b Avaya
avayaInit dmccHost dmccPort = makeSnaplet "avaya" "AVAYA" Nothing $ do
    addRoutes
      [ ("/ws/:ext", method GET $ avayaWsProxy dmccHost dmccPort)
      , ("/hook/", method POST hook)
      ]
    Avaya <$> (liftIO $ newTVarIO $ Map.empty)
