{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.EraGlonass.Model.CaseEraGlonassFailures.Persistent where

import           Data.Typeable
import           Data.Text (Text)
import           Data.Aeson

import           Database.Persist.TH
import           Database.Persist.Postgresql.JSON ()

import           Carma.EraGlonass.Model.CaseEraGlonassFailures.Types


-- | @CaseEraGlonassFailures@ persistent model.
mkPersist sqlSettings [persistLowerCase|
CaseEraGlonassFailures sql=CaseEraGlonassFailures
  integrationPoint EGIntegrationPoint sql=integrationpoint
  requestBody Value Maybe sql=requestbody
  comment Text Maybe sql=comment
  deriving Typeable Show
|]
