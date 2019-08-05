{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, ExplicitNamespaces #-}

module Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent where

import           GHC.Generics (type Generic)

import           Data.Typeable
import           Data.Text (type Text)
import           Data.Aeson

import           Database.Persist.TH
import           Database.Persist.Postgresql.JSON ()

import           Carma.Monad.Clock (type UTCTime)
import           Carma.EraGlonass.Types.EGRequestId (type EGRequestId)
import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( type EGIntegrationPoint
                   )


-- | @CaseEraGlonassFailure@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CaseEraGlonassFailure json sql=CaseEraGlonassFailure

  ctime            UTCTime            sql=ctime default=CURRENT_TIME
  integrationPoint EGIntegrationPoint sql=integrationpoint
  requestId        EGRequestId Maybe  sql=requestid
  requestBody      Value       Maybe  sql=requestbody
  responseBody     Value       Maybe  sql=responsebody
  comment          Text        Maybe  sql=comment

  deriving Generic Typeable Show
|]
