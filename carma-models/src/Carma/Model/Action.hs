module Carma.Model.Action where

import Data.Aeson as A
import Data.Text
import Data.Time.Clock (UTCTime)
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types (HMDiffTime)

import Carma.Model.ActionResult (ActionResult)
import Carma.Model.ActionType (ActionType)
import Carma.Model.Case (Case)
import Carma.Model.DeferTime (label, time)
import Carma.Model.Role (Role)
import Carma.Model.Service (Service)
import Carma.Model.Usermeta (Usermeta)

data Action = Action
  { ident       :: PK Int Action                    "Действие"
  , caseId      :: F (IdentI Case)                  "caseId" ""
  , serviceId   :: F (Maybe (IdentI Service))       "serviceId" ""
  , aType       :: F (IdentI ActionType)            "type" ""
  , duetime     :: F UTCTime                        "duetime" "Ожидаемое время выполнения"
  , comment     :: F (Maybe Text)                   "comment" "Комментарий"
  , deferBy     :: F (Maybe HMDiffTime)             "deferBy" "Отложить на"
  , result      :: F (Maybe (IdentI ActionResult))  "result" "Результат"
  , ctime       :: F UTCTime                        "ctime" ""
  , assignTime  :: F (Maybe UTCTime)                "assignTime" ""
  , openTime    :: F (Maybe UTCTime)                "openTime" ""
  , closeTime   :: F (Maybe UTCTime)                "closeTime" ""
  , assignedTo  :: F (Maybe (IdentI Usermeta))      "assignedTo" "Ответственный"
  , targetGroup :: F (IdentI Role)                  "targetGroup" "Роль"
  , parent      :: F (Maybe (IdentI Action))        "parent" "Предыдущее действие"
  } deriving Typeable

instance Model Action where
  type TableName Action = "actiontbl"
  modelInfo = mkModelInfo Action ident
  modelView = \case
    "" -> Just $ modifyView defaultView $
          [ invisible caseId
          , invisible serviceId
          , invisible aType
          , deferBy `completeWith` time
          , setMeta "dictionaryLabel"
            (A.String $ fieldName label) deferBy
          , infoText "defertime" deferBy
          , setMeta "addClass" "redirectOnChange" result
          , setMeta "dictionaryType" "BoUsersDict" assignedTo
          , invisible ctime
          , invisible assignTime
          , invisible openTime
          , invisible closeTime
          , invisible assignedTo
          , invisible targetGroup
          ]
    _  -> Nothing
