{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.CarModel.Persistent where

import           Data.Typeable
import           Data.Text (Text)

import           Database.Persist.TH

import           Carma.Model.CarMake.Persistent (CarMakeId)


-- | @CarModel@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CarModel sql=CarModel

  value Text sql=value
  -- ^ Used for __carma-mobile-server__ case creation API

  label Text sql=label
  info Text sql=info
  parent CarMakeId sql=parent
  synonyms [Text] Maybe sql=synonyms
  fdds Text Maybe sql=fdds

  deriving Typeable Show
|]
