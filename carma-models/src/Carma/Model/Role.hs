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

all            = Ident 1 :: Ident Role
local          = Ident 2 :: Ident Role
front          = Ident 3 :: Ident Role
back           = Ident 4 :: Ident Role
bo_control     = Ident 5 :: Ident Role
head           = Ident 6 :: Ident Role
parguy         = Ident 7 :: Ident Role
manager        = Ident 8 :: Ident Role
accManager     = Ident 9 :: Ident Role
partner        = Ident 10 :: Ident Role
supervisor     = Ident 11 :: Ident Role
director       = Ident 12 :: Ident Role
analyst        = Ident 13 :: Ident Role
psaanalyst     = Ident 14 :: Ident Role
account        = Ident 15 :: Ident Role
admin          = Ident 16 :: Ident Role
programman     = Ident 17 :: Ident Role
op_checker     = Ident 18 :: Ident Role
op_close       = Ident 19 :: Ident Role
op_dealer      = Ident 20 :: Ident Role
contract_admin = Ident 21 :: Ident Role
contract_user  = Ident 22 :: Ident Role
partners_user  = Ident 23 :: Ident Role
vwfake         = Ident 24 :: Ident Role
