{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.CarMake where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.PgTypes()


data CarMake = CarMake
  { ident    :: PK Int CarMake ""
  , value    :: F Text "value" "value"
  , label    :: F Text "label" "Марка"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  , fdds     :: F (Maybe Text) "fdds" "VEHICLE MAKE"
  }
  deriving Typeable


mkIdents [t|CarMake|]
 [ ("vw", 1)
 , ("sy", 48)
 ]

instance Model CarMake where
  type TableName CarMake = "CarMake"
  modelInfo = mkModelInfo CarMake ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
