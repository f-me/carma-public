{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.AvayaAES 
    ( Avayaplet
    , avayaAESInit
    ) where
import Control.Monad.State
import Data.Functor
import Data.Maybe

import Data.Configurator
import Data.Lens.Common
import Data.Lens.Template

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth

import Network.Avaya as A
import Network.Avaya.Action as A
import qualified Data.Aeson as AE
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.HashMap.Lazy as H

data Avayaplet b = Avayaplet
    { auth :: Lens b (Snaplet (AuthManager b))
    , _conf :: A.AvayaConfig
    }
makeLens ''Avayaplet

withAuth :: (MonadState (Avayaplet b1) (m b1 v), MonadSnaplet m) =>
            m b1 (AuthManager b1) b -> m b1 v b
withAuth = (gets auth >>=) . flip withTop

callHandler :: Handler b (Avayaplet b) ()
callHandler = do
    Just user <- withAuth currentUser
    let meta = userMeta user
    let Just (AE.String extension) = H.lookup "avayaExt" meta
    let Just (AE.String password) = H.lookup "avayaPass" meta
    number <- fromMaybe "" <$> getParam "phone"
    conf <- gets _conf 
    let conf' = conf { cExtension = extension, cPassword = password }
    liftIO $ print conf'
    liftIO $ print number
    liftIO $ do
      print conf'
      print number
      Right st' <- A.startAvaya conf'
      A.runAvayaAction st' (A.call $ BU.toString number)
    return ()

routes :: [(B.ByteString, Handler b (Avayaplet b) ())]
routes = [ ("/call", method POST callHandler) ]

avayaAESInit :: Lens b (Snaplet (AuthManager b)) -> SnapletInit b (Avayaplet b)
avayaAESInit topAuth =
    makeSnaplet "avaya-aes" "Avaya AES snaplet." Nothing $ do
      cfg <- getSnapletUserConfig
      host <- liftIO $ lookupDefault "127.0.0.1" cfg "host"
      port <- liftIO $ lookupDefault 4721 cfg "port"
      user <- liftIO $ lookupDefault "user" cfg "user"
      password <- liftIO $ lookupDefault "password" cfg "password"
      delay <- liftIO $ lookupDefault "5" cfg "delay"
      version <- liftIO $ lookupDefault "4.2" cfg "version"
      duration <- liftIO $ lookupDefault "180" cfg "duration"
      callServerIp <- liftIO $ lookupDefault "" cfg "callServerIp"
      addRoutes routes
      return $ Avayaplet topAuth $ 
                 A.AvayaConfig host (fromInteger port) user password delay
                      version duration callServerIp "" ""
