{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent where

import           Data.Typeable
import           Data.Time.Clock

import           Database.Persist.TH

import           Carma.Model.Case.Persistent (CaseId)
import           Carma.EraGlonass.Types.EGRequestId (EGRequestId)
import           Carma.EraGlonass.Types.EGRequestForServiceRequest
                   (EGRequestForServiceRequest)


-- | @CaseEraGlonassCreateRequest@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CaseEraGlonassCreateRequest sql=CaseEraGlonassCreateRequest
  ctime UTCTime sql=ctime default=CURRENT_TIME
  associatedCase CaseId sql=caseid
  requestId EGRequestId sql=requestid
  requestBody EGRequestForServiceRequest sql=requestbody

  deriving Typeable Show
|]
