{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Test.Helpers
     ( findSubstring
     , withTestingServer
     ) where

import           Test.Hspec

import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as ParsecText
import           Text.InterpolatedString.QM

import           Control.Arrow
import           Control.Applicative ((<|>))
import           Control.Concurrent
import           Control.Monad.Catch

import           System.Process
import           System.IO

import           Servant.Client

import           Carma.Utils.Operators


findSubstring :: Text -> ParsecText.Parser Text
findSubstring str =
  ParsecText.try (ParsecText.string str)
    <|> (ParsecText.anyChar *> findSubstring str)


-- | Wrapper that starts testing HTTP server in background.
--
-- It terminates it (testing HTTP server) when wrapped monad is done.
-- Testing server using new clean SQLite in-memory database.
withTestingServer :: Bool -> MVar () -> Expectation -> Expectation
withTestingServer withoutOwnServer locker runTest =
  if withoutOwnServer
     then runWithoutServer
     else runWithServer

  where
    substr = findSubstring "Running incoming server on"

    serverCmd =
      (proc "stack" ["exec", "carma-era-glonass-integration-test-server"])
        { std_in  = Inherit
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

    runWithoutServer = do
      () <- takeMVar locker -- lock
      finally runTest $
       putMVar locker () -- unlock

    runWithServer = do
      () <- takeMVar locker -- lock
      (Nothing, Just hOut, Just hErr, hProc) <- createProcess serverCmd
      (outLogMVar :: MVar Text) <- newEmptyMVar
      (errLogMVar :: MVar Text) <- newEmptyMVar

      _ <-
        let
          f :: Text -> IO ()
          f accumulator = do
            isReadable <- (&&) <$> (not <$> hIsEOF hErr) <*> hIsReadable hErr
            if not isReadable
               then putMVar errLogMVar accumulator
               else T.hGetLine hErr >>= \x -> f [qm| {accumulator}  {x}\n |]
        in
          forkIO $ f mempty

      let readLog :: Text -> IO ()
          readLog outLog = do
            getProcessExitCode hProc >>= \case
              Nothing -> pure ()
              Just x  -> do
                errLog <- takeMVar errLogMVar
                fail [qmb| Testing server is failed with exit code: {x}!
                           Error log:\n{errLog} |]

            isReadable <- (&&) <$> (not <$> hIsEOF hOut) <*> hIsReadable hOut
            if not isReadable
               then readLog outLog
                    -- ^ Recursive repeat to catch unexpected termination
               else T.hGetLine hOut
                      <&> (id &&& ParsecText.parseOnly substr)
                      >>= \case (l, Left  _) -> readLog [qm| {outLog}  {l}\n |]
                                (l, Right _) ->
                                  putMVar outLogMVar [qm| {outLog}  {l}\n |]
                                  -- ^ Server is ready, we're done here

      let testFailureHandler :: ServantError -> IO ()
          testFailureHandler exception = do
            _ <- terminateProcess hProc >> waitForProcess hProc
            errLog <- takeMVar errLogMVar
            outLog <- takeMVar outLogMVar

            fail [qmb|
              Test is failed because of test server response:
              \  {exception}
              Testing server stderr:
                {if errLog == mempty then "  <log is empty>\n" else errLog}\
              Testing server stdout:
                {if outLog == mempty then "  <log is empty>\n" else outLog}
            |]

      finally (readLog mempty >> (runTest `catch` testFailureHandler)) $ do
        _ <- terminateProcess hProc >> waitForProcess hProc
        putMVar locker () -- unlock
