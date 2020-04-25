{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module NotificationLoop
  ( processLocationRequests
  , selectPendingRequests
  , _NOTIFICATION_CHANNEL
  )
  where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (bracket, handle, SomeException)
import           Control.Monad (forever, void, liftM3)
import           Data.Text (Text)
import           Data.ByteString (ByteString)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import           Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import           Text.InterpolatedString.QM (qm, qn)

import           Logger (logMsg, Priority(..))
import           Types (AppContext(..))


-- | Listens to PG notifications in a loop and processes incoming requests.
-- It processes existing requests before waiting on new notifications.
processLocationRequests :: AppContext -> IO ()
processLocationRequests AppContext{pgUri, urlPrefix}
  = void $ forkIO $ foreverWithDelay 2000
    (\e -> logMsg Error [qm| processLocationRequests: {e} |])
    $ bracket
      (logMsg Info "Connecting to Postgres" >> PG.connectPostgreSQL pgUri)
      (\c -> logMsg Error "Closing Postgres connection" >> PG.close c)
      $ \c -> forever $ do
        selectPendingRequests c >>= mapM_ (\r -> do
          logMsg Info [qm| processing {r} |]
          processPendingRequest c urlPrefix r)
        -- 'execute "listen ?" [channel]' does not work
        -- as it is converted into syntactically incorrect query
        -- with channel name quoted: listen 'channel'.
        void $ PG.execute_ c [qm| listen {_NOTIFICATION_CHANNEL} |]
        void $ PG.getNotification c

-- | Repeat some action forever but have a little delay between repetitions.
-- NB. Delay in masked handler is not good but we expect no exceptions from
-- other threads so it's ok.
foreverWithDelay :: Int -> (SomeException -> IO ()) -> IO () -> IO ()
foreverWithDelay ms h = forever . handle (\e -> h e >> threadDelay (ms*1000))

-- | This is the name of the channel that we listen for notifications about new
-- location sharing requests.
_NOTIFICATION_CHANNEL :: ByteString
_NOTIFICATION_CHANNEL = "create_location_sharing_request"

data PendingRequest
  = PendingRequest { requestId :: Int, caseId :: Int, urlKey :: Text }
  deriving Show

-- FIXME: can be autoderived in postgresql-simple >= 0.6
instance FromRow PendingRequest where
  fromRow = liftM3 PendingRequest field field field

selectPendingRequests :: PG.Connection -> IO [PendingRequest]
selectPendingRequests c = PG.query_ c [qn|
  select r.id, caseId, urlKey
  \ from "LocationSharingRequest" r
  \ where smsId is null
  \   and validUntil > now()
  |]

-- | Processes request by composing and sending message to the client.
-- Also sends a notification that can be handled by the main service in
-- order to show something to call-center operator.
processPendingRequest :: PG.Connection -> Text -> PendingRequest -> IO ()
processPendingRequest c urlPrefix PendingRequest{..} = do
  let messageText = [qm|
        \ Заявка {caseId}. Перейдите по ссылке, чтобы отправить
        \ свои координаты: {urlPrefix}/{urlKey}\
        |]
  void $ PG.execute c
    "call send_sms_for_location_sharing_request(requestId := ?, message := ?)"
    (requestId, messageText :: Text)
