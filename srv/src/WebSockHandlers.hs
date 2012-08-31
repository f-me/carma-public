
module WebSockHandlers
  (runWebSockServer
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Error
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map

import Control.Concurrent
import Control.Concurrent.STM
import Network.WebSockets

import Avaya.Actions
import Avaya.MessageLoop
import Avaya.DeviceMonitoring
import qualified Avaya.Messages.Response as Rs
import qualified Avaya.Messages.Request as Rq


-- FIXME: credentials to config
runWebSockServer :: String -> Int -> IO ()
runWebSockServer ip port = do
  connMap <- newTVarIO Map.empty
  void $ forkIO $ runServer ip port (rqHandler connMap)

type AvayaMap
  = TVar (Map.Map
    (B.ByteString,B.ByteString)
    (LoopHandle,MonitoringHandle))

rqHandler :: AvayaMap -> Request -> WebSockets Hybi00 ()
rqHandler cMapVar rq = case B.split '/' $ requestPath rq of
  ["","avaya",ext,pwd] -> do
    cMap <- liftIO $ readTVarIO cMapVar
    Right (h,m) <- case Map.lookup (ext,pwd) cMap of
      Just hm -> return $ Right hm
      Nothing -> startMonitoring rq ext pwd
    liftIO $ atomically
      $ writeTVar cMapVar $! Map.insert (ext,pwd) (h,m) cMap

    acceptRequest rq
    s <- getSink
    liftIO $ attachObserver h $ evHandler s
    void $ runEitherT $ forever $ do
      msg <- lift receive
      case msg of
        ControlMessage (Close _) -> do
          liftIO $ do
            putStrLn $ "Sutdown session " ++ show (sessionId m)
            atomically $ modifyTVar' cMapVar $ Map.delete (ext,pwd)
            stopDeviceMonitoring h m
            shutdownLoop h
          left ()

        DataMessage (Text t) -> case L.split ':' t of
          ["dial", number] -> liftIO $ do
            sendRequestSync h
              $ Rq.SetHookswitchStatus
                {acceptedProtocol = actualProtocolVersion m
                ,device = deviceId m
                ,hookswitchOnhook = False
                }
            dialNumber h (actualProtocolVersion m) (deviceId m) (L.unpack number)

          ["acceptCall"]
            -> liftIO $ sendRequestAsync h
              $ Rq.SetHookswitchStatus
                {acceptedProtocol = actualProtocolVersion m
                ,device = deviceId m
                ,hookswitchOnhook = False
                }
          _ -> return ()
        _ -> return ()

  _ -> rejectRequest rq "401"


evHandler ws ev = case ev of
  AvayaRsp rsp -> case rsp of
    Rs.RingerStatusEvent{..} -> sendSink ws $ textData $ L.fromChunks
      ["{\"type\":\"ringer\",\"ringer\":\""
      ,T.encodeUtf8 ringMode
      ,"\"}"
      ]
    Rs.DisplayUpdatedEvent{..} -> sendSink ws $ textData $ L.fromChunks
      ["{\"type\":\"display\",\"display\":\""
      ,T.encodeUtf8 contentsOfDisplay
      ,"\"}"
      ]
    _ -> return ()
  _ -> return ()


startMonitoring rq ext pwd = do
  res <- liftIO $ startMessageLoop "192.168.20.5" 4721
  case res of
    Left _ -> rejectRequest rq "Can't start session"
    Right h -> do
      -- liftIO $ attachObserver h print
      res <- liftIO $ startDeviceMonitoring h
        "avaya" "avayapassword" "S8300ADAC"
        (T.decodeUtf8 ext) (T.decodeUtf8 pwd)
      case res of
        Left _ -> rejectRequest rq "Can't register device"
        Right m -> return $ Right (h,m)
