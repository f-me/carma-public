{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, ExplicitNamespaces #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent where

import           GHC.Generics (type Generic)

import           Data.Typeable
import           Data.Text (type Text)
import           Data.Aeson

import           Database.Persist.TH
import           Database.Persist.Postgresql.JSON ()

import           Carma.Monad.Clock (type UTCTime)
import           Carma.EraGlonass.Model.Types (PgArray)
import           Carma.EraGlonass.Types.EGRequestId (type EGRequestId)
import           Carma.EraGlonass.Types.EGIntegrationPoint
                   ( type EGIntegrationPoint
                   )

type ListOfTamestamps = PgArray UTCTime


-- | @CaseEraGlonassFailure@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CaseEraGlonassFailure json sql=CaseEraGlonassFailure

  ctime            UTCTime                  sql=ctime default=CURRENT_TIME
  integrationPoint EGIntegrationPoint       sql=integrationpoint
  requestId        EGRequestId        Maybe sql=requestid
  requestBody      Value              Maybe sql=requestbody
  responseBody     Value              Maybe sql=responsebody
  comment          Text               Maybe sql=comment
  repeats          ListOfTamestamps   Maybe sql=repeats

  deriving Generic Typeable Show
|]
