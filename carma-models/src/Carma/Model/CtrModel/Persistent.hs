{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.CtrModel.Persistent where

import           Data.Text (Text)
import           Data.Typeable

import           Database.Persist.TH


-- | @CtrModel@ persistent model.
mkPersist sqlSettings [persistLowerCase|
CtrModel sql=CtrModel
  value Text sql=value
  label Text sql=label

  deriving Typeable Show
|]
