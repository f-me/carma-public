{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent where

import           GHC.Generics (Generic)

import           Data.Typeable
import           Data.Time.Clock
import           Data.Aeson (Value)

import           Database.Persist.TH

import           Carma.Model.Case.Persistent (CaseId)
import           Carma.EraGlonass.Types.EGRequestId (EGRequestId)


-- | @CaseEraGlonassCreateRequest@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CaseEraGlonassCreateRequest sql=CaseEraGlonassCreateRequest
  ctime UTCTime sql=ctime default=CURRENT_TIME
  associatedCase CaseId sql=caseid
  requestId EGRequestId sql=requestid
  requestBody Value sql=requestbody

  deriving Typeable Generic Show
|]
