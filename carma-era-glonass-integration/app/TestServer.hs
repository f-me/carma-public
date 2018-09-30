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
import           Control.Concurrent.STM.TSem

import           System.IO

import           Database.Persist.Sqlite

import           Carma.Monad.LoggerBus
import           Carma.Monad.STM
import           Carma.Model.Usermeta.Persistent as Usermeta
import           Carma.Model.Program.Persistent as Program
import           Carma.Model.SubProgram.Persistent as SubProgram
import           Carma.Model.City.Persistent as City
import           Carma.Model.Case.Persistent as Case
import           Carma.Model.CaseSource.Persistent as CaseSource
import           Carma.Model.CaseStatus.Persistent as CaseStatus
import           Carma.Model.CarMake.Persistent as CarMake
import           Carma.Model.Engine.Persistent as Engine
import           Carma.EraGlonass.Types
import           Carma.EraGlonass.App
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Logger.LoggerForward (runLoggerForward)
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent
                   as CaseEraGlonassFailure
import           Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent
                   as CaseEraGlonassCreateRequest


main :: IO ()
main = do
  -- Needed for reading output in tests
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  app TestingAppMode $ \_ _ runServer -> do
    logInfo [qms| Creating in-memory SQLite database... |]
    loggerBus' <- ask

    flip runLoggerForward loggerBus' $
      withSqliteConn ":memory:" $ \sqliteConnection -> do

        let migrateModel (typeRep', migration) = do
              logDebug [qm| Applying migration for "{typeRep'}" model... |]
              runMigration migration `runReaderT` sqliteConnection

        mapM_ migrateModel
          [ ( typeRep (Proxy :: Proxy Usermeta)
            , Usermeta.migrateAll
            )
          , ( typeRep (Proxy :: Proxy Program)
            , Program.migrateAll
            )
          , ( typeRep (Proxy :: Proxy SubProgram)
            , SubProgram.migrateAll
            )
          , ( typeRep (Proxy :: Proxy City)
            , City.migrateAll
            )
          , ( typeRep (Proxy :: Proxy Case)
            , Case.migrateAll
            )
          , ( typeRep (Proxy :: Proxy CaseSource)
            , CaseSource.migrateAll
            )
          , ( typeRep (Proxy :: Proxy CaseStatus)
            , CaseStatus.migrateAll
            )
          , ( typeRep (Proxy :: Proxy CarMake)
            , CarMake.migrateAll
            )
          , ( typeRep (Proxy :: Proxy Engine)
            , Engine.migrateAll
            )
          , ( typeRep (Proxy :: Proxy CaseEraGlonassFailure)
            , CaseEraGlonassFailure.migrateAll
            )
          , ( typeRep (Proxy :: Proxy CaseEraGlonassCreateRequest)
            , CaseEraGlonassCreateRequest.migrateAll
            )
          ]


        logDebug [qns| Creating new testing plug "Program" and "SubProgram"
                       which are Era Glonass participants... |]

        flip runReaderT sqliteConnection $ do

          testingProgramId <-
            insert Program { programActive        = True
                           , programLabel         = "Testing Program plug"
                           , programClient        = Nothing
                           , programClientAddress = Nothing
                           , programClientCode    = Nothing
                           , programFdds          = Nothing
                           , programManagers      = Nothing
                           , programPType         = Nothing
                           , programHelp          = Nothing
                           }

          insert_ SubProgram { subProgramParent = testingProgramId

                             , subProgramLabel  = "Testing SubProgram plug"
                             , subProgramActive = True
                             , subProgramLeader = True

                             , subProgramSynonyms = Nothing

                             , subProgramDiagTree = Nothing

                             , subProgramMailAddr = Nothing
                             , subProgramMailPass = Nothing

                             , subProgramContacts = []
                             , subProgramServices = []

                             , subProgramCheckPeriod = Nothing
                             , subProgramValidFor    = Nothing

                             , subProgramDefaultMake = Nothing

                             , subProgramSmsSender  = ""
                             , subProgramSmsContact = ""
                             , subProgramSmsProgram = ""

                             , subProgramEraGlonassParticipant = True

                             , subProgramContractPrs = []

                             , subProgramTemplate = Nothing
                             , subProgramLogo     = Nothing

                             , subProgramHelp       = Nothing
                             , subProgramDealerHelp = Nothing
                             }


        logDebug [qm| Creating testing plug "Usermeta" "admin" {admin}... |]
        flip runReaderT sqliteConnection $ insertKey admin Usermeta


        logDebug [qms| Creating testing plug "CaseSource" "eraGlonass"
                       {eraGlonass}... |]

        flip runReaderT sqliteConnection $
          insertKey eraGlonass CaseSource.CaseSource
            { caseSourceLabel = "Era Glonass" }


        logDebug [qm| Creating testing plug "CaseStatus" "front" {front}... |]

        flip runReaderT sqliteConnection $
          insertKey front CaseStatus { caseStatusLabel = "front-office" }


        dbLock <- atomically $ newTSem 1
        lift $ runServer $ DBConnection dbLock sqliteConnection
