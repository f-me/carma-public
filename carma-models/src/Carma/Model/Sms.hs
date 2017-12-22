module Carma.Model.Sms where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Model
import Data.Typeable

import Data.Model.View
import Carma.Model.Types ()
import Carma.Model.Case (Case)
import Carma.Model.SmsTemplate (SmsTemplate)
import Carma.Model.LegacyTypes (Phone)
import Carma.Model.Usermeta (Usermeta)

data Sms = Sms
  { ident     :: PK Int Sms ""
  , ctime     :: F UTCTime "ctime" "Время создания сообщения"

  , mtime     :: F UTCTime
                   "mtime"
                   "Время последнего изменения сообщения (или статуса)"

  , status    :: F Text  "status"  "Статус отправки сообщения"
  , phone     :: F Phone "phone"   "Телефон получателя"
  , sender    :: F Text  "sender"  "Подпись отправителя"

  , caseRef   :: F (Maybe (IdentI Case))
                   "caseRef"
                   "Номер кейса"

  , userRef   :: F (Maybe (IdentI Usermeta))
                   "userRef"
                   "Пользователь, инициировавший отправку"

  , template  :: F (Maybe (IdentI SmsTemplate))
                   "template"
                   "Шаблон сообщения"

  , msgText   :: F Text
                   "msgText"
                   "Текст сообщения"

  , foreignId :: F (Maybe Text)
                   "foreignId"
                   "Идентификатор сообщения в sms-гейте"

  } deriving Typeable


instance Model Sms where
  type TableName Sms = "Sms"
  modelInfo = mkModelInfo Sms ident
  modelView = \case
    "" -> Just $ modifyView defaultView
      [ invisible ctime
      , invisible mtime
      , invisible status
      , invisible sender
      , textarea  msgText
      , regexp regexpPhone phone
      ]
    _  -> Nothing
