{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.Role where

import Prelude hiding (all, head)
import Data.Text (Text)
import Data.Typeable
import Data.Model
import Data.Model.View
import Data.Model.TH
import Carma.Model.Types()
import Carma.Model.PgTypes()

data Role = Role
  {ident  :: PK Int Role ""
  ,label  :: F Text "label"  "Название роли"
  ,value  :: F Text "value"  "Внутреннее название роли"
  -- ^ Internal label is only used to set screen permissions.
  ,isBack :: F Bool "isBack" "Роль бэкофиса"
  } deriving Typeable

mkIdents [t|Role|]
 [ ("core", 1)
 , ("call", 2)
 , ("parguy", 3)
 , ("userAdmin", 4)
 , ("userViewer", 5)
 , ("lovAdmin", 6)
 , ("lovViewer", 7)
 , ("reportManager", 8)
 , ("billManager", 9)
 , ("billChecker", 10)
 , ("vinAdmin", 11)
 , ("supervisor", 12)
 , ("head", 13)
 , ("back", 14)
 , ("psaanalyst", 15)
 , ("searchCase", 16)
 , ("searchCall", 17)
 , ("searchContract", 18)
 , ("partner", 19)
 , ("contract_admin", 20)

 , ("bo_qa", 22)
 , ("bo_order", 23)
 , ("bo_control", 24)
 , ("bo_account", 25)
 , ("bo_director", 26)
 , ("bo_analyst", 27)
 , ("bo_bill", 28)
 , ("bo_parguy", 29)
 , ("bo_close", 30)
 , ("bo_dealer", 31)

 , ("vwfake", 32)
 , ("front", 33)
 , ("dpViewer", 34)
 , ("programManager", 35)
 , ("bo_secondary", 41)
 ]

instance Model Role where
  type TableName Role = "Role"
  idents = Carma.Model.Role.idents
  modelInfo = mkModelInfo Role ident
  modelView = \case
    "" -> Just $ modifyView defaultView
                [ readonly value
                , infoText "roleValue" value
                ]
    _  -> Nothing
