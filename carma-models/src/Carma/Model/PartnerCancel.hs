module Carma.Model.PartnerCancel where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Model
import Data.Typeable

import Data.Model.View
import Carma.Model.Types()
import Carma.Model.Case (Case)
import Carma.Model.Partner (Partner)
import Carma.Model.Service (Service)
import Carma.Model.PartnerRefusalReason (PartnerRefusalReason)
import Carma.Model.Usermeta (Usermeta)

data PartnerCancel = PartnerCancel
  {ident    :: PK Int PartnerCancel ""
  ,ctime    :: F UTCTime            "ctime"     "Время создания"
  ,caseId   :: F (IdentI Case)      "caseId"    ""
  ,partnerId:: F (IdentI Partner)   "partnerId" ""
  ,serviceId:: F (IdentI Service)   "serviceId" ""
  ,partnerCancelReason
            :: F (IdentI PartnerRefusalReason)
                "partnerCancelReason"
                "Причина отказа"
  ,comment  :: F Text               "comment"   "Комментарии"
  ,owner    :: F (IdentI Usermeta)  "owner"     ""
  } deriving Typeable


instance Model PartnerCancel where
  type TableName PartnerCancel = "PartnerCancel"
  modelInfo = mkModelInfo PartnerCancel ident
  modelView = \case
    "" -> Just $ modifyView defaultView
      [ invisible ctime
      , hiddenIdent caseId
      , hiddenIdent partnerId
      , hiddenIdent serviceId
      , textarea  comment
      , hiddenIdent owner
      ]
    _  -> Nothing
