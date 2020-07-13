module Logger
  ( Priority(..)
  , logMsg
  , startLogThread
  )
  where

import           Control.Monad (forever, void, when, liftM2)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Concurrent (ThreadId, myThreadId, forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.Chan
import qualified Data.Text as S (Text, lines)
import qualified Data.Text.Lazy as L (Text, intercalate)
import qualified Data.Text.Lazy.Builder as B (toLazyText, fromText)
import qualified Data.Text.Lazy.Builder.Int as B (decimal)
import qualified Data.Text.Lazy.IO as L (hPutStrLn)
import           System.IO (stderr)
import           System.IO.Unsafe (unsafePerformIO)


-- | Log priority levels according to the RFC 5424 (The Syslog Protocol).
data Priority
  = Emergency | Alert | Crit | Error | Warn | Notice | Info | Debug
  deriving (Eq, Ord, Enum)

-- | Write a message to stderr.
-- We use strict text here to force message evaluation in the calling thread.
logMsg :: MonadIO m => Priority -> S.Text -> m ()
logMsg p msg = liftIO $ do
  -- Prepend each line with a numeric representation of the specified priority.
  -- This will be parsed by systemd (see 'man sd_journal_print' for more info).
  let prefix = "<" <> B.decimal (fromEnum p) <> ">"
  let msg' = L.intercalate "\n"
        $ map (B.toLazyText . (prefix <>) . B.fromText)
        $ S.lines $! msg
  -- Write directly to stderr if log thread has not started.
  isEmptyMVar logThread >>= \case
    True -> L.hPutStrLn stderr msg'
    False -> writeChan logChan msg'

-- | Fork a thread to write log messages to stderr.
-- All messages are serialized through a channel, this prevents interwining
-- messages from different threads.
-- FIXME: Should we install signal handler to flush queue before we die?
startLogThread :: IO ()
startLogThread = forkIO logLoop >>= void . tryPutMVar logThread

logThread :: MVar ThreadId
logThread = unsafePerformIO newEmptyMVar
{-# NOINLINE logThread #-}

logChan :: Chan L.Text
logChan = unsafePerformIO newChan
{-# NOINLINE logChan #-}

logLoop :: IO ()
logLoop = do
  -- Drop extra threads in case 'startLogThread' has been called several times.
  iAmTheOne <- liftM2 (==) myThreadId (readMVar logThread)
  when iAmTheOne
    $ forever $ readChan logChan >>= L.hPutStrLn stderr
