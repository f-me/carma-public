{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Test.Helpers
     ( findSubstring
     , withTestingServer
     , withTestingServerLogged
     ) where

import           Test.Hspec
import           Test.HUnit.Lang

import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as ParsecText
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Arrow
import           Control.Applicative ((<|>))
import           Control.Concurrent
import           Control.Monad.Catch

import           System.Process
import           System.IO

import           Servant.Client

import           Carma.Utils.Operators


{-|
A parser to find specified substring.

@
hasFoo :: Text -> Bool
hasFoo = either (const False) (const True) . parseOnly (findSubstring "foo")
@
-}
findSubstring :: Text -> ParsecText.Parser Text
findSubstring str =
  ParsecText.try (ParsecText.string str)
    <|> (ParsecText.anyChar *> findSubstring str)


-- | Wrapper that starts testing HTTP server in background.
--
-- It terminates it (testing HTTP server) when wrapped monad is done.
-- Testing server using new clean SQLite in-memory database.
--
-- As a result it returns absorbed __stdout__ and __stderr__ of the testing
-- server or @Nothing@ if testing server haven't been started.
withTestingServerLogged
  :: Bool -> MVar () -> Expectation -> IO (Maybe (Text, Text))
withTestingServerLogged withoutOwnServer locker runTest =
  if withoutOwnServer
     then Nothing <$ runWithoutServer
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
      () <- takeMVar locker -- Waiting for lock
      finally runTest $ putMVar locker () -- Unlock

    isPipeReadable pipeHandler =
      (&&) <$> (not <$> hIsEOF pipeHandler) <*> hIsReadable pipeHandler

    runWithServer = do
      () <- takeMVar locker -- Waiting for lock
      (Nothing, Just hOut, Just hErr, hProc) <- createProcess serverCmd
      (outLogMVar :: MVar Text) <- newEmptyMVar
      (errLogMVar :: MVar Text) <- newEmptyMVar

      _ <- let
        f :: Text -> IO ()
        f accumulator = do
          isReadable <- isPipeReadable hErr
          if not isReadable
             then putMVar errLogMVar accumulator
             else T.hGetLine hErr >>= \x -> f [qm| {accumulator}  {x}\n |]

        in forkIO $ f mempty

      let readLog :: Text -> IO ()
          readLog outLog = do
            getProcessExitCode hProc >>= \case
              Nothing -> pure ()
              Just x  -> do
                errLog <- takeMVar errLogMVar
                fail [qmb| Testing server is failed with exit code: {x}!
                           Error log:\n{errLog} |]

            isReadable <- isPipeReadable hOut
            if not isReadable
               then readLog outLog
                    -- ^ Recursive repeat to catch unexpected termination
               else T.hGetLine hOut
                      <&> (id &&& ParsecText.parseOnly substr)
                      >>= \case (l, Left  _) -> readLog [qm| {outLog}  {l}\n |]
                                (l, Right _) ->
                                  -- Server is ready.
                                  -- Reading rest of the log in background.
                                  void $ forkIO $
                                    readRestOfLog [qm| {outLog}  {l}\n |]
            where
              readRestOfLog :: Text -> IO ()
              readRestOfLog accumulator = do
                isReadable <- isPipeReadable hOut
                if not isReadable
                   then putMVar outLogMVar accumulator
                   else T.hGetLine hOut
                          >>= \x -> readRestOfLog [qm| {accumulator}  {x}\n |]

      let finishServer :: IO Text
          finishServer = do
            _ <- terminateProcess hProc >> waitForProcess hProc
            errLog <- takeMVar errLogMVar
            outLog <- takeMVar outLogMVar

            pure [qmb|
              Testing server stderr:
                {if errLog == mempty then "  <log is empty>\n" else errLog}\
              Testing server stdout:
                {if outLog == mempty then "  <log is empty>\n" else outLog}
            |]

      let serverFailureHandler :: ServantError -> IO ()
          serverFailureHandler exception = do
            absorbedLog <- finishServer

            fail [qmb|
              Test is failed because of test server response:
              \  {exception}
              {absorbedLog}
            |]

      let testFailureHandler :: HUnitFailure -> IO ()
          testFailureHandler exception = do
            absorbedLog <- finishServer

            -- Adding absorbed server log to the exception
            throwM $ case exception of
              HUnitFailure srcLoc failureReason ->
                HUnitFailure srcLoc $ case failureReason of
                  Reason msg ->
                    Reason [qm| {msg}\n{absorbedLog} |]

                  ExpectedButGot Nothing expected got ->
                    ExpectedButGot (Just [qm| {absorbedLog} |]) expected got

                  ExpectedButGot (Just comment) expected got ->
                    ExpectedButGot
                      (Just [qm| {comment}\n{absorbedLog} |]) expected got

      let testRunner =
            runTest
              `catch` serverFailureHandler
              `catch` testFailureHandler

      let returnLog = do
            _ <- terminateProcess hProc >> waitForProcess hProc
            errLog <- takeMVar errLogMVar
            outLog <- takeMVar outLogMVar
            pure $ Just (outLog, errLog)

      finally (readLog mempty >> testRunner >> returnLog) $ do
        terminateProcess hProc
        _ <- waitForProcess hProc
        putMVar locker () -- Unlock


-- | Wrapper that starts testing HTTP server in background.
--
-- It terminates it (testing HTTP server) when wrapped monad is done.
-- Testing server using new clean SQLite in-memory database.
withTestingServer :: Bool -> MVar () -> Expectation -> IO ()
withTestingServer withoutOwnServer locker runTest =
  void $ withTestingServerLogged withoutOwnServer locker runTest
