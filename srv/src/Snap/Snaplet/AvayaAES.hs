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
import Network.Avaya 
import Network.Avaya.Action

import qualified Data.Aeson as AE
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T

data Avayaplet b = Avayaplet
    { auth :: Lens b (Snaplet (AuthManager b))
    , _avayaState :: Maybe AvayaState
    }
makeLens ''Avayaplet

withAuth :: (MonadState (Avayaplet b1) (m b1 v), MonadSnaplet m) =>
            m b1 (AuthManager b1) b -> m b1 v b
withAuth = (gets auth >>=) . flip withTop


callHandler :: Handler b (Avayaplet b) ()
callHandler = do
    n <- fromMaybe "" <$> getParam "phone"
    Right () <- runAvayaActionWithLens avayaState (call $ BU.toString n)
    return ()


startSession :: Handler b (Avayaplet b) ()
startSession = do
    conf <- loadConfig
    Right ast <- liftIO $ startAvaya conf
    app <- get
    put $ setL avayaState (Just ast) app 
    return ()
  where
    getEP :: Handler b (Avayaplet b) (T.Text, T.Text)
    getEP = do
      Just user <- withAuth currentUser
      let meta = userMeta user
      let Just (AE.String exten) = H.lookup "avayaExt" meta
      let Just (AE.String pass) = H.lookup "avayaPass" meta
      return (exten, pass)

    loadConfig :: Handler b (Avayaplet b) AvayaConfig
    loadConfig = do
      cfg <- getSnapletUserConfig
      let fromConfig def par = liftIO $ lookupDefault def cfg par
      host <- fromConfig "127.0.0.1" "host"
      port <- fromConfig 4721 "port"
      username <- fromConfig "user" "user"
      password <- fromConfig "password" "password"
      delay <- fromConfig "5" "delay"
      version <- fromConfig "4.2" "version"
      duration <- fromConfig "180" "duration"
      callServerIp <- fromConfig "" "callServerIp"
      (exten, pass) <- getEP
      return $ AvayaConfig host (fromInteger port) username password delay
                           version duration callServerIp exten pass


routes :: [(B.ByteString, Handler b (Avayaplet b) ())]
routes = [ ("/call", method POST callHandler)
         , ("/start-session", method POST startSession)
         ]


avayaAESInit :: Lens b (Snaplet (AuthManager b)) -> SnapletInit b (Avayaplet b)
avayaAESInit topAuth =
    makeSnaplet "avaya-aes" "Avaya AES snaplet." Nothing $ do
      addRoutes routes
      return $ Avayaplet topAuth Nothing

