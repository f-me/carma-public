{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent where

import           Data.Typeable
import           Data.Text (Text)
import           Data.Aeson

import           Database.Persist.TH
import           Database.Persist.Postgresql.JSON ()

import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Types
import           Carma.Monad.Clock (UTCTime)


-- | @CaseEraGlonassFailure@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CaseEraGlonassFailure json sql=CaseEraGlonassFailure
  ctime UTCTime sql=ctime default=CURRENT_TIME
  integrationPoint EGIntegrationPoint sql=integrationpoint
  requestBody Value Maybe sql=requestbody
  comment Text Maybe sql=comment
  deriving Typeable Show
|]
