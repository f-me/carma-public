
module Carma.Model.Program where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types()

data Program = Program
  { ident                 :: PK Int Program
  , label                 :: F Text            "label"  "Название"
  , client                :: F (Maybe Text)    "client" "Заказчик"
  , clientAddress         :: F (Maybe Text)    "clientAddress" "Адрес заказчика"
  , clientCode            :: F (Maybe Text)    "clientCode" "Код заказчика"
  } deriving Typeable

instance Model Program where
  type TableName Program = "Program"
  modelInfo = mkModelInfo Program ident
  modelView _ = defaultView

-- known programs
-- citroen = Ident 56 :: Ident Program
-- peugeot = Ident 57 :: Ident Program
-- vwMotor = Ident 63 :: Ident Program
-- vwCargo = Ident 64 :: Ident Program
