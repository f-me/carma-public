{-# LANGUAGE TemplateHaskell #-}
module Carma.Model.CallerType where


import Data.Text (Text)
import Data.Typeable

import Carma.Model.Types()
import Carma.Model.PgTypes()

import Data.Model
import Data.Model.View
import Data.Model.TH

data CallerType = CallerType
  {ident   :: PK Int CallerType ""
  ,label   :: F Text "label" "Название цвета"
  } deriving Typeable

mkIdents [t|CallerType|]
 [("client",   1)
 ,("partner",  2)
 ,("dealer",   3)
 ,("employee", 4)
 ,("other",    5)
 ]

instance Model CallerType where
  type TableName CallerType = "CallerType"
  idents = Carma.Model.CallerType.idents
  modelInfo = mkModelInfo CallerType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
