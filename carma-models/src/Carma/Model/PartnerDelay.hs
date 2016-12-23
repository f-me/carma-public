module Carma.Model.PartnerDelay where

import Data.Aeson((.=), object)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Model
import Data.Typeable

import Data.Model.View
import Carma.Model.Types()
import Carma.Model.Case (Case)
import Carma.Model.Partner (Partner)
import Carma.Model.Service (Service)
import Carma.Model.Usermeta (Usermeta)
import Carma.Model.PartnerDelay.Reason (PartnerDelay_Reason)
import qualified Carma.Model.PartnerDelay.Reason as Reason
import Carma.Model.PartnerDelay.Notified (PartnerDelay_Notified)
import Carma.Model.PartnerDelay.Confirmed (PartnerDelay_Confirmed)
import qualified Carma.Model.PartnerDelay.Exceptional as Exceptional
import Carma.Model.PartnerDelay.Exceptional (PartnerDelay_Exceptional)

data PartnerDelay = PartnerDelay
  {ident              :: PK Int PartnerDelay ""
  ,ctime              :: F UTCTime            "ctime"     "Время создания"
  ,caseId             :: F (IdentI Case)      "caseId"    ""
  ,serviceId          :: F (IdentI Service)   "serviceId" ""
  ,partnerId          :: F (IdentI Partner)   "partnerId" ""
  ,owner              :: F (IdentI Usermeta)  "owner"     ""
  ,delayReason        :: F (IdentI PartnerDelay_Reason)
                           "delayReason"
                           "Причина опоздания"
  ,delayReasonComment :: F (Maybe Text)
                           "delayReasonComment"
                           "Описание причины опоздания"
  ,delayMinutes       :: F Int
                           "delayMinutes"
                           "Время опоздания в минутах"
  ,notified           :: F (IdentI PartnerDelay_Notified)
                           "notified"
                           "Партнёр предупредил об опоздании"
  ,delayConfirmed     :: F (IdentI PartnerDelay_Confirmed)
                           "delayConfirmed"
                           "Продолжаем с текущим партнёром"
  ,exceptional        :: F (IdentI PartnerDelay_Exceptional)
                           "exceptional"
                           "Исключительный случай"
  ,exceptionalComment :: F (Maybe Text)
                           "exceptionalComment"
                           "Описание случая"
  } deriving Typeable


instance Model PartnerDelay where
  type TableName PartnerDelay = "PartnerDelay"
  modelInfo = mkModelInfo PartnerDelay ident
  modelView = \case
    "" -> Just $ modifyView defaultView
      [ invisible ctime
      , hiddenIdent caseId
      , hiddenIdent partnerId
      , hiddenIdent serviceId
      , hiddenIdent owner
      , textarea delayReasonComment
      , setMeta "visibleIf" (object ["delayReason" .= [Reason.other]]) delayReasonComment
      , textarea exceptionalComment
      , setMeta "visibleIf" (object ["exceptional" .= [Exceptional.yes]]) exceptionalComment
      , required delayReason
      , required delayReasonComment
      , required delayMinutes
      , required notified
      , required delayConfirmed
      , required exceptional
      , required exceptionalComment
      ]
    _  -> Nothing

