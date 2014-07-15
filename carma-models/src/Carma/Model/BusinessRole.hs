{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.BusinessRole where

import Data.Vector (Vector)
import Data.Text (Text)
import Data.Typeable
import Data.Model
import Data.Model.View
import Data.Model.TH
import Carma.Model.Role (Role)
import Carma.Model.Types()
import Carma.Model.PgTypes()

data BusinessRole = BusinessRole
  {ident  :: PK Int BusinessRole ""
  ,label  :: F Text "label"  "Название бизнес-роли"
  ,roles  :: F (Maybe (Vector (IdentI Role))) "roles" "Роли"
  } deriving Typeable

mkIdents [t|BusinessRole|]
  [ ("front",       1) -- Front Office
  , ("backOrder",   2) -- Back Office: Заказ услуг
  , ("backSecond",  3) -- Back Office: Заказ вторичных услуг
  , ("backControl", 4) -- Back Office: Контроль услуг
  ]

instance Model BusinessRole where
  type TableName BusinessRole = "BusinessRole"
  modelInfo = mkModelInfo BusinessRole ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
