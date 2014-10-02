{-# LANGUAGE ExistentialQuantification #-}

module Trigger.Dsl.SendWsMessage where

import Control.Monad.IO.Class
import Data.Typeable
import GHC.TypeLits

import Data.Model as Model
import Data.Model.Patch (Patch)

import Application (AppHandler)
import Snaplet.Messenger (sendMessage)
import Snaplet.Messenger.Class (withMsg)
import Utils.LegacyModel (mkLegacyIdent)
import Utils.Events (logLegacyCRUD)
import Carma.Model.Event (EventType(Update))


data WsMessageField m = forall fld . SendWsMessage m fld => WsMessageField fld

class SendWsMessage m fld where
  sendWsMsg :: fld -> IdentI m -> Patch m -> AppHandler ()

instance (Model m, KnownSymbol name, Typeable typ)
    => SendWsMessage m (m -> F typ name opt)
  where
    sendWsMsg fld i p = do
      let j = mkLegacyIdent i
      withMsg $ sendMessage j p
      logLegacyCRUD Update j fld
