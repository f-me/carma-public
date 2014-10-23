module Carma.Model.KPI.Stat where


import           Data.Typeable

import           Data.Time.Clock (DiffTime)
import           Data.Time.Calendar (Day)
import qualified Data.Aeson as Aeson

import           Data.Model
import           Data.Model.View

import           Carma.Model.Usermeta (Usermeta)

data StatKPI = StatKPI
  { frontIdent   :: PK Int StatKPI     "KPI пользователя"
  , user         :: F (IdentI Usermeta) "userid"          "Оператор"
  , day          :: F Day              "day"     "Дата"

  , inReady      :: F (Maybe DiffTime) "Ready"   "Готов"
  , inBusy       :: F (Maybe DiffTime) "Busy"    "Занят"
  , inDinner     :: F (Maybe DiffTime) "Dinner"  "Обед"
  , inRest       :: F (Maybe DiffTime) "Rest"    "Перерыв"
  , inServiceBreak
                 :: F (Maybe DiffTime) "ServiceBreak" "Служебный перерыв"
  , inLoggedOut  :: F (Maybe DiffTime) "LoggedOut"    "Разлогинен"
  , totalRest      :: F (Maybe DiffTime) "totalRest"     "Всего в перерывах"
  , totalLoggedIn  :: F (Maybe DiffTime) "totalLoggedIn" "Всего в системе"

  , infoTime     :: F (Maybe DiffTime) "infoTime"  "В разговоре: информационные"
  , infoCount    :: F (Maybe Int)      "infoCount" "Количество: информационные"

  , procTime  :: F (Maybe DiffTime) "procTime"  "В разговоре: Обработка кейса"
  , procCount :: F (Maybe Int)      "procCount" "Количество: Обработка кейса"

  , newTime  :: F (Maybe DiffTime) "newTime"  "В разговоре: Создание кейса"
  , newCount :: F (Maybe Int)      "newCount" "Количество: Создание кейса"

  , controlT :: F (Maybe DiffTime)
                "controlT" "Ср. время \"Контроль услуги\""
  , controlC :: F (Maybe Int)
                "controlC" "Действий \"Контроль услуги\""
  , orderServiceT :: F (Maybe DiffTime)
                "orderServiceT" "Ср. время \"Заказ услуги\""
  , orderServiceC :: F (Maybe Int)
                "orderServiceC" "Действий \"Заказ услуги\""
  , tellMeMoreT :: F (Maybe DiffTime)
                "tellMeMoreT" "Ср. время \"Заказ услуги - доп. инф.\""
  , tellMeMoreC :: F (Maybe Int)
                "tellMeMoreC" "Действий \"Заказ услуги - доп. инф.\""
  , callMeMaybeT :: F (Maybe DiffTime)
                "callMeMaybeT" "Ср. время \"Заказ услуги - моб. прил.\""
  , callMeMaybeC :: F (Maybe Int)
                "callMeMaybeC" "Действий \"Заказ услуги - моб. прил.\""

  , assigned
    :: F (Maybe Int) "assigned" "Назначено"
  , assignedOverdue
    :: F (Maybe Int) "assigned_overdue" "Просрочено из назначенных"
  , closed
    :: F (Maybe Int) "closed" "Выполнено"
  , closedOverdue
    :: F (Maybe Int) "closed_overdue" "Просрочено из выполненных"
  , unclosed
    :: F (Maybe Int) "unclosed" "Не выполнено"
  , unclosedOverdue
    :: F (Maybe Int) "unclosed_overdue" "Просрочено из не выполненных"

  , calltime    :: F (Maybe DiffTime) "calltime"    "Итого: Время в разговоре"
  , callAmount  :: F (Maybe Int)      "callAmount"  "Итого: Количество звонков"
  , callAvgtime :: F (Maybe DiffTime) "callAvgTime" "Среднее время разговора"

  , utilization :: F (Maybe Double) "utilization" "Утилизация"
  , avgActOverdue
    :: F (Maybe DiffTime) "avgActionOverdue" "Ср. время просроч. действия"

  , actionsRelation :: F (Maybe Double) "actionsRelation" "Отношение: Действия"
  , timeRelation    :: F (Maybe Double) "timeRelation" "Отношение: Время"

  , actionsAmount
    :: F (Maybe Int) "actionsAmount" "Итого действий"
  , actionsAvgtime
    :: F (Maybe DiffTime) "actionsAvgtime" "Ср. время обработки действия"
} deriving Typeable

instance Model StatKPI where
  type TableName StatKPI = "StatKPI"
  modelInfo = mkModelInfo StatKPI frontIdent
  modelView = \case
    "kpi" -> Just $ modifyView (stripId $ defaultView)
      [setMeta "dictionaryLabel" (Aeson.String "realName") user]
    _     -> Nothing

