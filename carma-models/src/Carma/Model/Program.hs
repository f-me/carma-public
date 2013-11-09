
module Carma.Model.Program where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View


data Program = Program
  { ident                 :: PK Int Program
  , label                 :: F Text            "label"  ""
  , value                 :: F Text            "value"  ""
  , active                :: F Bool            "active" ""
  , client                :: F Text            "client" ""
  , clientCode            :: F Text            "clientCode" ""
  , clientAddress         :: F Text            "clientAddress" ""
  } deriving Typeable

instance Model Program where
  type TableName Program = "programtbl"
  modelInfo = mkModelInfo Program ident
  modelView _ = defaultView

-- known programs
-- citroen = Ident 56 :: Ident Program
-- peugeot = Ident 57 :: Ident Program
-- vwMotor = Ident 63 :: Ident Program
-- vwCargo = Ident 64 :: Ident Program
