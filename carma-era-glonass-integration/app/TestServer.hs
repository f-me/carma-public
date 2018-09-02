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

import           Database.Persist.Sqlite

import           Carma.Monad.LoggerBus
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.App
import           Carma.EraGlonass.Logger ()
import           Carma.EraGlonass.Logger.LoggerForward (runLoggerForward)
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent
                   as CaseEraGlonassFailure


main :: IO ()
main = app TestingAppMode $ \_ runServer -> do
  logInfo [qms| Creating in-memory SQLite database... |]
  loggerBus' <- ask

  flip runLoggerForward loggerBus' $
    withSqliteConn ":memory:" $ \sqliteConnection -> do

      let migrateModel (trep, migration) = do
            logDebug [qm| Applying migration for "{trep}" model... |]
            runMigration migration `runReaderT` sqliteConnection

      mapM_ migrateModel
        [ ( typeRep (Proxy :: Proxy CaseEraGlonassFailure)
          , CaseEraGlonassFailure.migrateAll
          )
        ]

      lift $ runServer $ DBConnection sqliteConnection
