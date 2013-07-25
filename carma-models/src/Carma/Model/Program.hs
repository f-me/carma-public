
module Carma.Model.Program where

import Data.Text
import Data.Model
import Data.Typeable


data Program = Program
  { ident                 :: F (Ident Program) "id"     ""
  , active                :: F Bool            "active" ""
  , label                 :: F Text            "label"  ""
--  , client                :: Field "client"
--  , clientCode            :: Field "clientCode"
--  , clientAddress         :: Field "clientAddress"
--  , services              :: Field "services"
--  , carCheckPeriodDefault :: Field "carCheckPeriodDefault"
--  , carCheckPeriodDefault :: Field "duedateDefault"
--  , contracts             :: Field "contracts"
--  , programPermissions    :: Field "programPermissions"
--  , vinFormat             :: Field "vinFormat"
--  , logo                  :: Field "logo"
--  , help                  :: Field "help"
  } deriving Typeable

instance Model Program where
  type TableName Program = "programtbl"

-- known programs
citroen = Ident 56 :: Ident Program
peugeot = Ident 57 :: Ident Program
vwMotor = Ident 63 :: Ident Program
vwCargo = Ident 64 :: Ident Program
