{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ServiceType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.Types()
import Carma.Model.PgTypes()

import Carma.Model.CtrModel (CtrModel)

data ServiceType = ServiceType
  { ident
    :: PK Int ServiceType "Тип услуги"
  , label
    :: F Text "label" "Тип"
  , icon
    :: F Text "icon"  "Иконка"
  , fdds
    :: F Text "fdds"  "FDDS-код"
  , model
    :: F (IdentI CtrModel) "model"  "Модель данных"
  } deriving Typeable

mkIdents [t|ServiceType|]
 [ ("tech", 1)
 , ("towage", 2)
 , ("rent", 3)
 , ("hotel", 4)
 , ("taxi", 5)
 , ("sober", 6)
 , ("transportation", 7)
 , ("deliverCar", 8)
 , ("deliverParts", 9)
 , ("ken", 10)
 , ("tech1", 11)
 , ("information", 12)
 , ("consultation", 13)
 , ("tickets", 14)
 , ("tripOn", 15)
 , ("medic", 16)
 , ("deliverClient", 17)
 , ("adjuster", 18)
 ]

instance Model ServiceType where
  type TableName ServiceType = "ServiceType"
  idents = Carma.Model.ServiceType.idents
  modelInfo = mkModelInfo ServiceType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
