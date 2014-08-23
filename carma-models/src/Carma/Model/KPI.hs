module Carma.Model.KPI where

import           Data.Typeable

import           Data.Time.Clock (DiffTime)
import qualified Data.Aeson as Aeson

import           Data.Model
import           Data.Model.View

import           Carma.Model.Usermeta (Usermeta)
import           Carma.Model.Case     (Case)
-- import           Carma.Model.Types    (UserStateVal)

data StatKPI = StatKPI
  { frontIdent   :: PK Int StatKPI     "KPI пользователя"
  , user         :: F (IdentI Usermeta) "userid"          "Оператор"
  -- , currentState :: F UserStateVal      "currentState"  "Текущий статус"

  -- , inCurrent    :: F DiffTime "inCurrent" "Время"
  , inReady      :: F (Maybe DiffTime) "Ready"   "Готов"
  , inBusy       :: F (Maybe DiffTime) "Busy"    "Занят"
  , inDinner     :: F (Maybe DiffTime) "Dinner"  "Обед"
  , inRest       :: F (Maybe DiffTime) "Rest"    "Перерыв"
  , inServiceBreak
                 :: F (Maybe DiffTime) "ServiceBreak" "Служебный перерыв"
  , inLoggedOut  :: F (Maybe DiffTime) "LoggedOut"    "Разлогинен"
  , totalRest      :: F (Maybe DiffTime) "totalRest"     "Всего в перерывах"
  , totalLoggedIn  :: F (Maybe DiffTime) "totalLoggedIn" "Всего в системе"

  , infoTime     :: F DiffTime "infoTime"  "В разговоре: информационные"
  , infoCount    :: F Int      "infoCount" "Количество: информационные"

  , procTime  :: F DiffTime "procTime"  "В разговоре: Обработка кейса"
  , procCount :: F Int      "procCount" "Количество: Обработка кейса"

  , newTime  :: F DiffTime "newTime"  "В разговоре: Создание кейса"
  , newCount :: F Int      "newCount" "Количество: Создание кейса"

  -- , callTime :: F DiffTime "callTime" "Итого: Время в разговоре"
  -- , amount   :: F Int      "amount"   "Итого: Количество звонков"
  -- , avgTime  :: F DiffTime "avgTime"  "Итого: Количество звонков"
 } deriving Typeable

instance Model StatKPI where
  type TableName StatKPI = "StatKPI"
  modelInfo = mkModelInfo StatKPI frontIdent
  modelView = \case
    "kpi" -> Just $ modifyView (stripId $ defaultView)
      [setMeta "dictionaryLabel" (Aeson.String "realName") user]
    _     -> Nothing

