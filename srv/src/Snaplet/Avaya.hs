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
import           Data.Configurator as Cfg
import           Data.Functor
import           Data.Maybe

import           Data.ByteString as BS
import qualified Data.Map as Map
import           Data.Text as Text

import           Network.WebSockets
import           Network.WebSockets.Snap

import           Snap

import           Data.Model
import           Carma.Model.Usermeta

import           AppHandlers.Util


data Avaya = Avaya
    { extMap     :: TVar (Map.Map (IdentI Usermeta) Text)
    , dmccWsHost :: Maybe Text
    , dmccWsPort :: Int
    }


routes :: [(ByteString, Handler b Avaya ())]
routes = [ ("/ws/:ext", method GET avayaWsProxy)
         , ("/hook/", method POST hook)
         ]


-- | Proxy requests to/from dmcc-ws Web Socket.
avayaWsProxy :: Handler a Avaya ()
avayaWsProxy= do
  ext <- fromMaybe (error "No extension specified") <$> getIntParam "ext"
  avayaConn <- liftIO newEmptyTMVarIO
  dmccWsHost' <- gets dmccWsHost
  dmccWsPort' <- gets dmccWsPort
  let dmccWsHost'' = Text.unpack $ fromMaybe "localhost" dmccWsHost'
      -- Client <-> CaRMa
      serverApp pending = do
        conn <- acceptRequest pending
        srvThread <- forkIO $ do
          avayaConn' <- liftIO $ atomically $ takeTMVar avayaConn
          forever $ receive conn >>= send avayaConn'
        flip finally (killThread srvThread) $
          runClient dmccWsHost'' dmccWsPort' ("/" ++ show ext) (proxyApp conn)
      -- CaRMa <-> dmcc-ws
      proxyApp serverConn conn = do
        liftIO $ atomically $ putTMVar avayaConn conn
        forever $ receive conn >>= send serverConn
  runWebSocketsSnap serverApp


hook :: Handler a Avaya ()
hook = error "Not implemented yet"


avayaInit :: SnapletInit b Avaya
avayaInit = makeSnaplet "avaya" "AVAYA" Nothing $ do
  addRoutes routes
  cfg <- getSnapletUserConfig
  liftIO $ Avaya
    <$> newTVarIO Map.empty
    <*> Cfg.lookup cfg "dmcc-ws-host"
    <*> Cfg.lookupDefault 8333 cfg "dmcc-ws-port"
