module Carma.Model.Event (Event(..), EventType(..)) where

import Data.Typeable
import Data.Time.Clock (UTCTime)
import Data.Text
import Data.Aeson

import Data.Model
import Data.Model.View

import Carma.Model.Usermeta (Usermeta)
import Carma.Model.Types


data Event = Event
  { ident     :: PK Int Event                    "Событие"
  , userid    :: F (IdentI Usermeta) "userid"    "Пользователь"
  , ctime     :: F UTCTime           "ctime"     "Дата создания"
  , eventType :: F EventType         "type"      "Тип события"
  , modelId   :: F Int               "modelId"   ""
  , modelName :: F Text              "modelName" "Модель"
  , field     :: F (Maybe Text)      "field"     "Поле"
  , patch     :: F (Maybe Value)     "patch"     ""
  } deriving Typeable

instance Model Event where
  type TableName Event = "Event"
  modelInfo = mkModelInfo Event ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing

