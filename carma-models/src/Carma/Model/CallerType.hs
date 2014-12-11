module Carma.Model.CallerType where


import Data.Text (Text)
import Data.Typeable

import Carma.Model.Types()
import Carma.Model.PgTypes()

import Data.Model
import Data.Model.View

data CallerType = CallerType
  {ident   :: PK Int CallerType ""
  ,label   :: F Text "label" "Название цвета"
  } deriving Typeable

instance Model CallerType where
  type TableName CallerType = "CallerType"
  modelInfo = mkModelInfo CallerType ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
