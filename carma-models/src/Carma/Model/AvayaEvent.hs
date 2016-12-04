module Carma.Model.AvayaEvent where

import Data.Text
import Data.Time.Clock
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types ()
import Carma.Model.PgTypes ()

import Carma.Model.Action
import Carma.Model.AvayaEventType
import Carma.Model.Usermeta


data AvayaEvent = AvayaEvent
  { ident
    :: PK Int AvayaEvent "Событие AVAYA"
  , ctime
    :: F UTCTime "ctime" "Дата/время"
  , eType
    :: F (IdentI AvayaEventType) "eType" "Тип события"
  , operator
    :: F (IdentI Usermeta) "operator" "Сотрудник"
  , currentAction
    :: F (IdentI Action) "currentAction" "Действие"
  , interlocutors
    :: F (Vector Text) "interlocutors" "Собеседники"
  , callId
    :: F Text "callId" "Идентификатор звонка"
  } deriving Typeable


instance Model AvayaEvent where
  type TableName AvayaEvent = "AvayaEvent"
  modelInfo = mkModelInfo AvayaEvent Carma.Model.AvayaEvent.ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
