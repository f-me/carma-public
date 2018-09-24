{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.EraGlonass.Model.CaseEraGlonassCreateRequest.Persistent where

import           Data.Typeable
import           Data.Time.Clock
import           Data.Text

import           Database.Persist.TH

import           Carma.Model.Case.Persistent (CaseId)
import           Carma.EraGlonass.Types.RequestId (RequestId)
import           Carma.EraGlonass.Types.EGCallCardId (EGCallCardId)
import           Carma.EraGlonass.Types.EGCreateCallCardRequest
                   (EGCreateCallCardRequest)


-- | @CaseEraGlonassCreateRequest@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CaseEraGlonassCreateRequest sql=CaseEraGlonassCreateRequest
  ctime UTCTime sql=ctime default=CURRENT_TIME
  associatedCase CaseId sql=caseid
  requestId RequestId sql=requestid
  callCardId EGCallCardId sql=callcardid
  responseId Text sql=responseid
  requestBody EGCreateCallCardRequest sql=requestbody

  deriving Typeable Show
|]
