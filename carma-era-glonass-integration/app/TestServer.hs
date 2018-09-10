{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Testing server implementation (with in-memory SQLite database)
module Main (main) where

import           Data.Proxy
import           Data.Typeable
import           Text.InterpolatedString.QM

import           Control.Monad.Reader
import           Control.Monad.Trans.Class (lift)

import           System.IO

import           Database.Persist.Sqlite

import           Carma.Monad.LoggerBus
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.App
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Logger.LoggerForward (runLoggerForward)
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent
                   as CaseEraGlonassFailure


main :: IO ()
main = do
  -- Needed for reading output in tests
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  app TestingAppMode $ \_ runServer -> do
    logInfo [qms| Creating in-memory SQLite database... |]
    loggerBus' <- ask

    flip runLoggerForward loggerBus' $
      withSqliteConn ":memory:" $ \sqliteConnection -> do

        let migrateModel (typeRep', migration) = do
              logDebug [qm| Applying migration for "{typeRep'}" model... |]
              runMigration migration `runReaderT` sqliteConnection

        mapM_ migrateModel
          [ ( typeRep (Proxy :: Proxy CaseEraGlonassFailure)
            , CaseEraGlonassFailure.migrateAll
            )
          ]

        lift $ runServer $ DBConnection sqliteConnection
