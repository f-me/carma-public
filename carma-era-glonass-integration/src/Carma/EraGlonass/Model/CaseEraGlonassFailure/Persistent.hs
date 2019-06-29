{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, QuasiQuotes, TemplateHaskell #-}

module Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent where

import           Data.Typeable
import           Data.Text (Text)
import           Data.Aeson

import           Database.Persist.TH
import           Database.Persist.Postgresql.JSON ()

import           Carma.Monad.Clock (UTCTime)
import           Carma.EraGlonass.Types.EGRequestId (EGRequestId)
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types


-- | @CaseEraGlonassFailure@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CaseEraGlonassFailure json sql=CaseEraGlonassFailure
  ctime UTCTime sql=ctime default=CURRENT_TIME
  integrationPoint EGIntegrationPoint sql=integrationpoint
  requestId EGRequestId Maybe sql=requestid
  requestBody Value Maybe sql=requestbody
  responseBody Value Maybe sql=responsebody
  comment Text Maybe sql=comment

  deriving Typeable Show
|]
