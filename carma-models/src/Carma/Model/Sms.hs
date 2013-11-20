
module Carma.Model.Sms where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Model
import Data.Typeable

import Data.Model.View
import Carma.Model.Types()
import Carma.Model.SmsTemplate (SmsTemplate)


data Sms = Sms
  {ident    :: PK Int Sms
  ,ctime    :: F UTCTime      "ctime"    "Время создания сообщения"
  ,mtime    :: F UTCTime      "mtime"    "Время последнего изменения сообщения (или статуса)"
  ,status   :: F Text         "status"   "Статус отправки сообщения"
  ,caseRef  :: F (Maybe Text) "caseRef"  "Номер кейса"
  ,phone    :: F Text         "phone"    "Телефон получателя"
  ,sender   :: F Text         "sender"   "Подпись отправителя"
  ,template :: F (IdentI SmsTemplate)
                              "template" "Шаблон сообщения"
  ,msgText  :: F Text         "msgText"  "Текст сообщения"
  } deriving Typeable


instance Model Sms where
  type TableName Sms = "Sms"
  modelInfo = mkModelInfo Sms ident
  modelView _ = modifyView defaultView
    [invisible ctime
    ,invisible mtime
    ,invisible status
    ,invisible sender
    ,textarea  msgText
    ]
