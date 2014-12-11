{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.CallType where


import Data.Text (Text)
import Data.Typeable

import Carma.Model.Types()
import Carma.Model.PgTypes()

import Data.Model
import Data.Model.View
import Data.Model.TH

data CallType = CallType
  {ident   :: PK Int CallType ""
  ,label   :: F Text "label" "Название цвета"
  } deriving Typeable

mkIdents [t|CallType|]
  [ ("info", 1)
  , ("newCase", 2)
  , ("secondCall", 3)
  ]


instance Model CallType where
  type TableName CallType = "CallType"
  idents = Carma.Model.CallType.idents
  modelInfo = mkModelInfo CallType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
