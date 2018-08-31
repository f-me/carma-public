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


-- | @CaseEraGlonassFailure@ persistent model.
mkPersist sqlSettings [persistLowerCase|
CaseEraGlonassFailure json sql=CaseEraGlonassFailure
  integrationPoint EGIntegrationPoint sql=integrationpoint
  requestBody Value Maybe sql=requestbody
  comment Text Maybe sql=comment
  deriving Typeable Show
|]
