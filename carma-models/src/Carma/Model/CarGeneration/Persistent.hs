{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.CarGeneration.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH

import           Carma.Model.CarModel.Persistent (CarModelId)


-- | @CarGeneration@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CarGeneration sql=CarGeneration
  label    Text         sql=label
  parent   CarModelId   sql=parent
  synonyms [Text] Maybe sql=synonyms

  deriving Typeable Show
|]
