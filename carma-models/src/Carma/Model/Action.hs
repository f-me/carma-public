module Carma.Model.Action where

import Data.Text
import Data.Time.Clock (UTCTime)
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.ActionResult (ActionResult)
import Carma.Model.ActionType (ActionType)
import Carma.Model.Case (Case)
import Carma.Model.DeferTime (time)
import Carma.Model.LegacyTypes
import Carma.Model.Role (Role)
import Carma.Model.Usermeta (Usermeta)

data Action = Action
  { ident       :: PK Int Action                    "Действие"
  , parent      :: F (Maybe Reference)              "parentId" ""
  , caseId      :: F (IdentI Case)                  "caseId" ""
  , name        :: F (IdentI ActionType)            "name" ""
  , duetime     :: F UTCTime                        "duetime" "Ожидаемое время выполнения"
  , comment     :: F (Maybe Text)                   "comment" "Комментарий"
  , deferBy     :: F (Maybe Text)                   "deferBy" "Отложить на"
  , result      :: F (Maybe (IdentI ActionResult))  "result" "Результат"
  , ctime       :: F UTCTime                        "ctime" ""
  , assignTime  :: F (Maybe UTCTime)                "assignTime" ""
  , openTime    :: F (Maybe UTCTime)                "openTime" ""
  , closeTime   :: F (Maybe UTCTime)                "closeTime" ""
  , assignedTo  :: F (Maybe (IdentI Usermeta))      "assignedTo" "Ответственный"
  , targetGroup :: F (IdentI Role)                  "targetGroup" "Роль"
  , closed      :: F Bool                           "closed" "Закрыто"
  } deriving Typeable

instance Model Action where
  type TableName Action = "actiontbl"
  modelInfo = mkModelInfo Action ident
  modelView = \case
    "" -> Just $ modifyView defaultView $
          [ deferBy `completeWith` time
          , infoText "defertime" deferBy
          ]
    _  -> Nothing
