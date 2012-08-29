
module WebSockHandlers
  (runWebSockServer
  ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Concurrent
import Network.WebSockets

import Avaya.MessageLoop
import Avaya.DeviceMonitoring
import qualified Avaya.Messages.Response as Rs
import qualified Avaya.Messages.Request as Rq


-- FIXME: credentials to config
runWebSockServer :: String -> Int -> IO ()
runWebSockServer ip port
  = void . forkIO $ runServer ip port rqHandler

rqHandler :: Request -> WebSockets Hybi00 ()
rqHandler rq = case B.split '/' $ requestPath rq of
  ["","avaya",ext,pwd] -> do
    res <- liftIO $ startMessageLoop "192.168.20.5" 4721
    (h,(actualProtocolVersion,device)) <- case res of
      Left _ -> rejectRequest rq "Can't start session"
      Right h -> do
        liftIO $ attachObserver h print
        res <- liftIO $ startDeviceMonitoring h
          "avaya" "avayapassword" "S8300ADAC"
          (T.decodeUtf8 ext) (T.decodeUtf8 pwd)
        case res of
          Left _ -> rejectRequest rq "Can't register device"
          Right r -> return (h,r)

    acceptRequest rq
    getSink >>= liftIO . attachObserver h . evHandler
    forever $ do
      msg <- receive
      case msg of
        DataMessage (Text "acceptCall") -> liftIO $ sendRequestAsync h
          $ Rq.SetHookswitchStatus
            {acceptedProtocol = actualProtocolVersion
            ,device = device
            ,hookswitchOnhook = False
            }
        _ -> return ()

  _ -> rejectRequest rq "401"


evHandler ws ev = case ev of
  AvayaRsp rsp -> case rsp of
    Rs.RingerStatusEvent{..} -> sendSink ws $ DataMessage $ Text $ L.fromChunks
      ["{\"type\":\"ringer\",\"ringer\":\""
      ,T.encodeUtf8 ringMode
      ,"\"}"
      ]
    Rs.DisplayUpdatedEvent{..} -> sendSink ws $ DataMessage $ Text $ L.fromChunks
      ["{\"type\":\"display\",\"display\":\""
      ,T.encodeUtf8 contentsOfDisplay
      ,"\"}"
      ]
    _ -> return ()
  _ -> return ()
