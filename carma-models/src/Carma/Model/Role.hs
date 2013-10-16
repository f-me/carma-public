module Carma.Model.Role where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View


data Role = Role
  { label :: F Text "label"  "Название роли"
  , value :: F Text "value"  "Внутреннее название роли"
  } deriving Typeable


instance Model Role where
  type TableName Role = "Role"
  modelInfo = mkModelInfo Role
  modelView _ = modifyView defaultView [readonly value]
