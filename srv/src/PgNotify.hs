{-# LANGUAGE ScopedTypeVariables #-}
module PgNotify
  where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (bracket, handle, SomeException)
import           Control.Monad (forever, void)
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import           Data.Scientific (Scientific)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import           Snaplet.Messenger (Messenger, sendMessageIO)
import           Text.InterpolatedString.QM (qm, qn)

startLoop :: Messenger -> ByteString -> IO ()
startLoop msg pgUri = void $ forkIO $ foreverWithDelay 2000
  $ bracket (PG.connectPostgreSQL pgUri) PG.close
    $ \c -> forever $ do
      processResponses c msg
      void $ PG.execute_ c "listen new_location_sharing_response"
      void $ PG.getNotification c

foreverWithDelay :: Int ->  IO () -> IO ()
foreverWithDelay ms
  = forever
  . handle (\(_::SomeException) -> threadDelay (ms*1000))

processResponses :: PG.Connection -> Messenger -> IO ()
processResponses c msg
  = PG.query_ c [qn|
      select id, caseId, lon, lat
      \ from "LocationSharingResponse"
      \ where not processed
    |]
  >>= mapM_ (\(rspId, caseId, lon, lat) -> do
    void $ PG.execute c [qn|
      update casetbl
      \  set caseAddress_coords = (? || ',' || ?)
      \  where id = ?
    |] (lon :: Scientific, lat :: Scientific, caseId :: Int)

    void $ PG.execute c [qn|
      update "LocationSharingResponse"
      \  set processed = true
      \  where id = ?
    |] [rspId :: Int]

    sendMessageIO msg
      [qm|LocationSharing:{caseId}|]
      $ Aeson.object
        [("location", Aeson.object
          [("lon", Aeson.toJSON lon)
          ,("lat", Aeson.toJSON lat)
          ]
        )]
  )
