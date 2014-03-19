
module Carma.Model.Service.Tech where

import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Service (Service)
import Carma.Model.LegacyTypes


data Tech = Tech
  { ident    :: PK Int Tech ""
  , techType :: F (Maybe (IdentT TechTypes)) "techType" "Услуга"
  }
  deriving Typeable

instance Model Tech where
  type TableName Tech = "techtbl"
  type Parent Tech = Service
  modelInfo = mkModelInfo Tech ident
  modelView _ = Just $ (defaultView :: ModelView Tech) {mv_title = "Техпомощь"}
