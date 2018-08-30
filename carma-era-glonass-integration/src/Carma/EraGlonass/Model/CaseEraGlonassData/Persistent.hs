{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.EraGlonass.Model.CaseEraGlonassData.Persistent where

import           Data.Typeable

import           Database.Persist.TH

import           Carma.Model.Case.Persistent (CaseId)


-- | @CaseEraGlonassData@ persistent model.
mkPersist sqlSettings [persistLowerCase|
CaseEraGlonassData sql=CaseEraGlonassData
  associatedCase CaseId sql=caseid
  deriving Typeable Show
|]
