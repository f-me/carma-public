{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, ExplicitNamespaces #-}

module Carma.Model.EraGlonassCaseStatusUpdate.Persistent where

import           GHC.Generics (type Generic)

import           Data.Typeable (type Typeable)
import           Data.Text (type Text)
import           Data.Time.Clock (type UTCTime)

import           Database.Persist.TH

import           Carma.Model.LegacyTypes (type Phone)
import           Carma.Model.Case.Persistent (type CaseId)
import           Carma.Model.CaseStatus.Persistent (type CaseStatusId)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
EraGlonassCaseStatusUpdate json sql=EraGlonassCaseStatusUpdate

  ctime         UTCTime       sql=ctime       default=CURRENT_TIME
  caseId        CaseId        sql=caseid
  newCaseStatus CaseStatusId  sql=newcasestatus
  isProcessed   Bool          sql=isprocessed default=False
  processTime   UTCTime Maybe sql=processtime

  -- In case some of these values have changed since a request has been received
  customerName  Text    Maybe sql=customername
  customerPhone Phone   Maybe sql=customerphone
  terminalPhone Phone   Maybe sql=terminalphone

  deriving Generic Typeable Show
|]
