
module Carma.Model.Service.Tech where


import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Service (Service)


data Tech = Tech
  { ident    :: PK Int Tech ""
  , techType :: F Text {-FIXME-} "techType" "Услуга"
  }
  deriving Typeable

instance Model Tech where
  type TableName Tech = "techtbl"
  type Parent Tech = Service
  modelInfo = mkModelInfo Tech ident
  modelView _ = (defaultView :: ModelView Tech) {mv_title = "Техпомощь"}
