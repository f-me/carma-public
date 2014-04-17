module Carma.Model.Action where

import Data.Text
import Data.Time.Clock (UTCTime)
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Role (Role)
import Carma.Model.LegacyTypes

data Action = Action
  { ident       :: PK Int Action                    "Действие"
  , parentId    :: F (Maybe Text)                   "parentId" ""
  , caseId      :: F (Maybe Reference)              "caseId" ""
  , name        :: F (Maybe (IdentT ActionNames))   "name" ""
  , description :: F (Maybe Text)                   "description" ""
  , duetime     :: F (Maybe UTCTime) "duetime" "Ожидаемое время выполнения"
  , comment     :: F (Maybe Text)                   "comment" "Комментарий"
  , deferBy     :: F (Maybe (IdentT DeferTimes))    "deferBy" "Отложить на"
  , result      :: F (Maybe (IdentT ActionResults)) "result" "Результат"
  , ctime       :: F (Maybe UTCTime)                "ctime" ""
  , mtime       :: F (Maybe UTCTime)                "mtime" ""
  , assignTime  :: F (Maybe UTCTime)                "assignTime" ""
  , openTime    :: F (Maybe UTCTime)                "openTime" ""
  , closeTime   :: F (Maybe UTCTime)                "closeTime" ""
  , assignedTo  :: F (Maybe Text)                   "assignedTo" "Ответственный"
  , targetGroup :: F (Maybe (IdentT Role))          "targetGroup" "Роль"
  , priority    :: F (Maybe Text)                   "priority" "Приоритет"
  , closed      :: F (Maybe Bool)                   "closed" "Закрыто"
  } deriving Typeable

instance Model Action where
  type TableName Action = "actiontbl"
  modelInfo = mkModelInfo Action ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
