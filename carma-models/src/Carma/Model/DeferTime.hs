{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.DeferTime where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types()
import Carma.Model.PgTypes()

data DeferTime = DeferTime
  { ident    :: PK Int DeferTime   "Интервал откладывания действия"
  , time     :: F Text "time" "Интервал"
  , label    :: F Text "label" "Метка"
  } deriving Typeable

instance Model DeferTime where
  type TableName DeferTime = "DeferTime"
  modelInfo = mkModelInfo DeferTime ident
  modelView = \case
    "" -> Just $ modifyView defaultView [ infoText "defertime" time
                                        , regexp "timespan" time]
    _  -> Nothing
