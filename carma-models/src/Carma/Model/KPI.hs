module Carma.Model.KPI where

import           Data.Typeable

import           Data.Time.Clock (DiffTime)

import           Data.Model
import           Data.Model.View

import           Carma.Model.Usermeta (Usermeta)
import           Carma.Model.Case     (Case)
import           Carma.Model.Types    (UserStateVal)

data BaseKPI = BaseKPI
  { baseIdent    :: PK Int BaseKPI      "KPI пользователя"
  , user         :: F (IdentI Usermeta) "user"          "Оператор"
  , currentState :: F UserStateVal      "currentState"  "Текущий статус"

  , inCurrent    :: F DiffTime "inCurrent" "Время"
  , inReady      :: F DiffTime "inReady"   "Готов"
  , inBusy       :: F DiffTime "inBusy"    "Занят"
  , inDinner     :: F DiffTime "inDinner"  "Обед"
  , inRest       :: F DiffTime "inRest"    "Перерыв"
  , inServiceBreak
                 :: F DiffTime "inServiceBreak" "Служебный перерыв"
  , allRest      :: F DiffTime "inRest"         "Всего в перерывах"
  , loggedIn     :: F DiffTime "inRest"         "Всего в системе"
  } deriving Typeable

instance Model BaseKPI where
  type TableName BaseKPI = "no table"
  modelInfo = mkModelInfo BaseKPI baseIdent
  modelView = \case
    "" -> Just $ modifyView (defaultView) [invisible baseIdent]
    _  -> Nothing


data FrontKPI = FrontKPI
  { frontIdent   :: PK Int FrontKPI     "KPI пользователя"

  , infoCall     :: F DiffTime "infoCallTime"  "В разговоре: информационные"
  , infoCount    :: F Int      "infoCallCount" "Количество: информационные"

  , caseCall     :: F DiffTime "infoCallTime"  "В разговоре: Обработка кейса"
  , caseCount    :: F Int      "infoCallCount" "Количество: Обработка кейса"

  , newCall      :: F DiffTime "infoCallTime"  "В разговоре: Создание кейса"
  , newCount     :: F Int      "infoCallCount" "Количество: Создание кейса"

  , totalCall    :: F DiffTime "totalCall"  "Итого: Время в разговоре"
  , totalCount   :: F Int      "totalCount" "Итого: Количество звонков"
  } deriving Typeable

instance Model FrontKPI where
  type TableName FrontKPI = "FrontKPI"
  type Parent    FrontKPI = BaseKPI
  modelInfo = mkModelInfo FrontKPI frontIdent
  modelView = \case
    "kpi" -> Just $ modifyView (defaultView) [invisible frontIdent]
    _     -> Nothing


data OrderKPI = OrderKPI
  { orderIdent    :: PK Int OrderKPI "KPI пользователя"
  , currentCase   :: F (IdentI Case) "currentCase" "Номер кейса"

  , ordersCount   :: F Int      "ordersCount"   "Заказ услуги"
  , ordersAvgTime :: F DiffTime "ordersAvgTime" "Среднее время"

  , tellMeMoreCount   :: F Int "tellMeMoreCount" "Требуется доп. информация"
  , tellMeMoreAvgTime :: F DiffTime "tellMeMoreAvgTime" "Среднее время"

  , callMeMaybeCount   :: F Int "callMeMaybeCount" "Через мобильное приложение"
  , callMeMaybeAvtTime :: F DiffTime "callMeMaybeAvtTime" "Среднее время"

  , totalOrderCount
      :: F Int "totalCount"  "Итого: Выполненных действий"
  , totalOrderAvgTime
      :: F DiffTime "totalAvgTime" "Итого: Среднее время обработки"

  , orderUtil :: F Int "util" "Утилизация (Back)"
  } deriving Typeable

instance Model OrderKPI where
  type TableName OrderKPI = "OrderKPI"
  type Parent    OrderKPI = BaseKPI
  modelInfo = mkModelInfo OrderKPI orderIdent
  modelView = \case
    "" -> Just $ modifyView (defaultView) [invisible orderIdent]
    _  -> Nothing


data ControlKPI = ControlKPI
  { controlIdent :: PK Int ControlKPI ""
  , assignedControlCount
    :: F Int "assignedControlCount" "Итого: Назначено \"Контроль услуги\""
  , overdue
    :: F Int "overdue" "Просрочено из назначенных"
  , finishedControl
    :: F Int "finishedControl ""Итого: Выполнено (Back-Office: Контроль услуг)"
  , finishedOverdue
    :: F Int "finishedOverdue" "Просрочено из выполненных"
  , notFinished
    :: F Int "notFinished" "Итого: Не выполнено \"Контроль услуги\""
  , notFinishedOverdue
    :: F Int "notFinishedOverdue" "Просрочено из не выполненных"
  , totalAvg
    :: F DiffTime "totalAvg" "Среднее время обработки \"Контроль услуги\""
  , controlUtil
    :: F Int "util" "Утилизация"
  , overdueAvg
    :: F DiffTime "overdueAvg" "Среднее время просроченности действия"
  , actionsFrac
    :: F Int "actionsFrac" "Отношение: Действия"
  , avgTimeFrac
    :: F DiffTime "avgTimeFrac" "Отношение: Время"
  } deriving Typeable

instance Model ControlKPI where
  type TableName ControlKPI = "ControlKPI"
  type Parent    ControlKPI = BaseKPI
  modelInfo = mkModelInfo ControlKPI controlIdent
  modelView = \case
    "" -> Just $ modifyView (defaultView) [invisible controlIdent]
    _  -> Nothing
