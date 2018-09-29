{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.Diagnostics.Part.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH

import           Carma.Model.Diagnostics.System.Persistent (SystemId)


-- | @Part@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Part sql=Part
  parent SystemId sql=parent
  label Text sql=label
  fdds Text Maybe sql=fdds
  deriving Typeable Show
|]
