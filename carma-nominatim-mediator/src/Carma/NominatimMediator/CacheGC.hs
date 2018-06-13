-- This modules claens outdated cached responses.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Carma.NominatimMediator.CacheGC where

import           Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Time.Clock as Time
import qualified Data.Text as T
import           Text.InterpolatedString.QM

import           Control.Monad
import           Control.Monad.Reader.Class (MonadReader, asks)

import           Carma.NominatimMediator.Types
import           Carma.NominatimMediator.Utils
import           Carma.NominatimMediator.Logger


-- Cleans outdated cached responses.
-- Supposed to be run in own thread.
cacheGCInit
  :: ( MonadReader AppContext m
     , LoggerBusMonad m
     , IORefWithCounterMonad m
     , TimeMonad m
     , DelayMonad m
     )
  => Float
  -> Float
  -> m ()
cacheGCInit gcIntervalInHours cacheItemLifetimeInHours = do
  logInfo
    [qmb| Cache garbage collector is initialized.
          Intervals between checks is {gcIntervalInHours} hour(s).
          Cached response lifetime is {cacheItemLifetimeInHours} hour(s). |]

  forever $ do
    logInfo [qn| Cache garbage collector goes... |]
    currentTime <- getCurrentTime

    outdatedItems <-
      asks responsesCache >>=
        flip atomicModifyIORefWithCounter'
          (M.partition $ fst
                       ? Time.diffUTCTime currentTime
                       ? round
                       ? (<= cacheItemLifetime))

    when (M.size outdatedItems > 0) $
      logInfo [qms| These responses is outdated
                    and they were removed from cache.
                    They will be requested again next time.
                    Request params of responses
                    which were removed from cache:\
                    { T.pack $ mconcat
                    $ show ? ("\n  - " <>) <$> M.keys outdatedItems
                    } |]

    logInfo [qms| Cache garbage collector will wait for
                  {cacheGCIntervalInMinutes} minute(s)
                  before next check... |]

    delay cacheGCInterval

  where
    gcIntervalInSeconds = gcIntervalInHours * 60 * 60 :: Float
    cacheGCInterval = round $ secondInMicroseconds * gcIntervalInSeconds :: Int

    cacheGCIntervalInMinutes =
      round $ fromIntegral cacheGCInterval / secondInMicroseconds / 60 :: Int

    cacheItemLifetimeInSeconds = cacheItemLifetimeInHours * 60 * 60 :: Float
    cacheItemLifetime = round $ 24 * cacheItemLifetimeInSeconds :: Int
