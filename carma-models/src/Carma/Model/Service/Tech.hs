module Carma.Model.Service.Tech where

import qualified Data.Aeson as Aeson

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Service (Service)
import Carma.Model.TechType (TechType)


data Tech = Tech
  { ident          :: PK Int Tech ""
  , techType       :: F (Maybe (IdentI TechType)) "techType" "Услуга"
  , suburbanMilage :: F (Maybe Text) "suburbanMilage"
                      "Пробег техпомощи за городом"
  , orderNumber    :: F (Maybe Text) "orderNumber" "Номер заказ-наряда"
  }
  deriving Typeable

instance Model Tech where
  type TableName Tech = "techtbl"
  type Parent Tech = Service
  modelInfo = mkModelInfo Tech ident `withLegacyName` "tech"
  modelView v = case parentView v :: Maybe (ModelView Tech) of
    Nothing -> Nothing
    Just mv ->
      Just $ modifyView (mv {mv_title = "Техпомощь"})
             [ setMeta "dictionaryStringify" (Aeson.Bool True) techType ]
