{-# LANGUAGE OverloadedStrings #-}

{-|

Avaya snaplet provides interface with dmcc-ws via hooks and Web
Sockets.

- Web Socket proxy binds used Avaya extensions to Snap user ids.

- hooks allow DMCC to push information directly to CaRMa

-}

module Snaplet.Avaya
    ( Avaya(..)
    , avayaInit
    )

where

import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Functor

import           Data.ByteString as BS
import qualified Data.Map as Map

import           Data.Text as Text

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth


data Avaya = Avaya
    { extMap :: TVar (Map.Map UserId Text)
    }


routes :: [(ByteString, Handler b Avaya ())]
routes = [ ("/ws/:ext", method GET avayaWsProxy)
         , ("/hook/", method POST hook)
         ]


avayaWsProxy = error "Not implemented yet"


hook = error "Not implemented yet"



avayaInit :: SnapletInit b Avaya
avayaInit = makeSnaplet "avaya" "AVAYA" Nothing $ do
    addRoutes routes
    Avaya <$> (liftIO $ newTVarIO $ Map.empty)
