module Carma.Model.Service.Tech where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Service (Service)
import Carma.Model.TechType (TechType)


data Tech = Tech
  { ident       :: PK Int Tech ""
  , techType    :: F (Maybe (IdentI TechType)) "techType" "Услуга"
  , orderNumber :: F (Maybe Text) "orderNumber" "Номер заказ-наряда"
  }
  deriving Typeable

instance Model Tech where
  type TableName Tech = "techtbl"
  type Parent Tech = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo Tech ident
  modelView v = case parentView v :: Maybe (ModelView Tech) of
    Nothing -> Nothing
    Just mv ->
      Just $ modifyView (mv {mv_title = "Техпомощь"}) []
