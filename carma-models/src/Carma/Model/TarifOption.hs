module Carma.Model.TarifOption where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.PgTypes()
import Carma.Model.Search (searchView, one)
import Carma.Model.Partner (Partner)
import Carma.Model.PartnerService (PartnerService)


data TarifOption = TarifOption
  { ident     :: PK Int TarifOption ""
  , partnerId :: F (IdentI Partner) "parentId" "Услуга"
  , parentId  :: F (IdentI PartnerService) "parentId" "Услуга"
  , option    :: F Text "optionname" "Марка"
  , price1    :: F Text "price1" "Цена 1"
  , price2    :: F Text "price2" "Цена 2"
  }
  deriving Typeable


instance Model TarifOption where
  type TableName TarifOption = "TarifOption"
  modelInfo = mkModelInfo TarifOption ident
  modelView = \case
    "" -> Just defaultView
    "parents" -> Just
      $ (searchView
        [("partnerId", one partnerId), ("parentId", one parentId)])
        {mv_modelName = "TarifOption"}
    _  -> Nothing
