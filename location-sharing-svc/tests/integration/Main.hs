import Test.Hspec

import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket)
import           Control.Monad (void)
import           Data.ByteString (ByteString)
import qualified Database.PostgreSQL.Simple as PG
import           Text.InterpolatedString.QM (qn)

import           Types(AppContext(..))
import           NotificationLoop
  ( processLocationRequests
  , selectPendingRequests
  )

-- NB. Beware that Sms'es are created during this test.
-- Ensure that sms-svc is disabled and created messages are cleared.


-- | PostgreSQL connection string to use for testing.
uri :: ByteString
uri = "postgres://location_sharing_svc:pass@/carma"

-- | Random delay that should be enough to process requests
processingDelay :: IO ()
processingDelay = threadDelay (3 * 10^(6 :: Int))

-- TODO: what if there is no phone number in casetbl?
-- it "creates SMS while processing pending requests" $ do
-- it "sends notification upon SMS creation" $ do


main :: IO ()
main = hspec $ do

  let cxt = AppContext
        { pgUri = uri
        , pgPool = undefined
        , httpPort  = undefined
        , urlPrefix = "hello"
        , indexTpl  = undefined
        }

  let shouldBeDone = do
        processingDelay
        rqs <- withPG selectPendingRequests
        length rqs `shouldBe` 0

  describe "Notification loop" $ do
    it "has something to do before it is started" $ do
      -- add couple of requests
      rqs0 <- withPG selectPendingRequests
      withPG (\c -> createRequest c >> createRequest c)
      rqs1 <- withPG selectPendingRequests
      length rqs1 `shouldBe` 2 + length rqs0

    it "processes existing request without waiting for notification" $ do
      processLocationRequests cxt
      shouldBeDone

    it "unblocks the loop when new request arrives" $ do
      withPG createRequest
      shouldBeDone

    it "recovers from lost connection" $ do
      -- Drop pg connection in the notification loop.
      void $ withPG (\c -> PG.query_ c [qn|
        select pg_terminate_backend(pid)
        \ from pg_stat_activity
        \ where usename = 'location_sharing_svc'
        |] :: IO [[Bool]])
      withPG createRequest
      shouldBeDone


withPG :: (PG.Connection -> IO a) -> IO a
withPG = bracket (PG.connectPostgreSQL uri) PG.close


createRequest :: PG.Connection -> IO ()
createRequest c = do
  -- Select random case without location request.
  [[caseId]] <- PG.query_ c [qn|
    select id from casetbl c
    \ where not exists (
    \   select 1 from "LocationSharingRequest" r
    \     where r.caseId = c.id)
    \ limit 1
    |]
  void (PG.query c
    "select create_location_sharing_request(?, 1) is null"
    [caseId :: Int]
    :: IO [[Bool]])
