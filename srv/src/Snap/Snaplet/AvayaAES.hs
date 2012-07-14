
module Snap.Snaplet.AvayaAES 
    ( Avayaplet
    , avayaAESInit
    ) where

import Control.Monad.State
import Control.Applicative

import Data.Configurator

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Session
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Class

import Network.Avaya as A
import Network.Avaya.Action as A
import qualified Data.ByteString.Char8 as B


data Avayaplet b = Avayaplet
    {_conf :: A.AvayaConfig
    }


callHandler :: HasAuth b => Handler b (Avayaplet b) ()
callHandler = do
    sess <- withAuth $ gets session
    Just ext <- withTop sess $ getFromSession "avayaExt"
    Just pwd <- withTop sess $ getFromSession "avayaPwd"
    Just number <- getParam "number"
    conf <- gets _conf 
    let conf' = conf {cExtension = ext, cPassword = pwd}
    liftIO $ do
      Right st' <- A.startAvaya conf'
      let '+':'7':number' = B.unpack number
      A.runAvayaAction st' $ A.call ("98" ++ number') >> A.cleanup
    writeLBS ""


avayaAESInit :: HasAuth b => SnapletInit b (Avayaplet b)
avayaAESInit =
    makeSnaplet "avaya-aes" "Avaya AES snaplet" Nothing $ do
      addRoutes [ ("/call", method POST callHandler) ]
      cfg <- getSnapletUserConfig
      connectionInfo <- liftIO $ A.AvayaConfig
          <$> lookupDefault "127.0.0.1" cfg "host"
          <*> (fromInteger <$> lookupDefault 4721 cfg "port")
          <*> lookupDefault "user" cfg "user"
          <*> lookupDefault "password" cfg "password"
          <*> lookupDefault "5" cfg "delay"
          <*> lookupDefault "4.2" cfg "version"
          <*> lookupDefault "180" cfg "duration"
          <*> lookupDefault "" cfg "callServerIp"
          <*> pure "" <*> pure ""
      return $ Avayaplet connectionInfo
