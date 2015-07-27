module Carma.Model.KPI.Group where

import           Data.Typeable

import           Data.Time.Clock (DiffTime)

import           Data.Model
import           Data.Model.View

import           Carma.Model.Types()
import           Carma.Model.PgTypes()

data GroupKPI = GroupKPI
  { ident       :: PK Int GroupKPI     "KPI пользователя"

  , infoTime     :: F (Maybe DiffTime) "infoTime"  "В разговоре: информационные"
  , infoCount    :: F (Maybe Int)      "infoCount" "Количество: информационные"

  , procTime  :: F (Maybe DiffTime) "procTime"  "В разговоре: Обработка кейса"
  , procCount :: F (Maybe Int)      "procCount" "Количество: Обработка кейса"

  , newTime  :: F (Maybe DiffTime) "newTime"  "В разговоре: Создание кейса"
  , newCount :: F (Maybe Int)      "newCount" "Количество: Создание кейса"

  , calltime    :: F (Maybe DiffTime) "calltime"    "Итого: Время в разговоре"
  , callAmount  :: F (Maybe Int)      "callAmount"  "Итого: Количество звонков"
  , callAvgtime :: F (Maybe DiffTime) "callAvgTime" "Среднее время разговора"

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

  , totalActionsAmount :: F (Maybe Int)
                "totalActionsAmount" "Итого: Выполненных действий"
  , totalActionsAvgTime :: F (Maybe DiffTime)
                "totalActionsAvgTime" "Итого: Среднее время обработки действия"

  , doneServices :: F (Maybe Int)
                 "doneServices" "Количество оказанных услуг всего"

  , allServicesAmount
    :: F (Maybe Int) "allServicesAmount" "Количество созданных услуг"
  , allServicesAvgAwaitingTime
    :: F (Maybe DiffTime) "allServicesAvgAwaitingTime" "Среднее время ожидания"
  , allServicesAvgRenderTime
    :: F (Maybe DiffTime) "allServicesAvgRenderTime" "Среднее время оказания"

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

  , utilization :: F (Maybe Double) "utilization" "Утилизация"
  , avgSrvProcessing :: F (Maybe DiffTime)
    "avgSrvProcessing" "Среднее время первичной обработки услуги"
  , avgSrvFinish :: F (Maybe DiffTime)
    "avgSrvFinish"
    "Среднее время разгрузки/окончания услуги по эвакуации/техпомощи"
  , satisfiedClients :: F (Maybe Int)
    "satisfiedClients" "Процент довольных клиентов"
  , claimsCount :: F (Maybe Int) "claimsCount" "Количество услуг с претензиями"
  , towStartAvgTime :: F (Maybe Double)
    "towStartAvgTime" "Среднее время прибытия эвакуатора/техпомощи"
  } deriving Typeable

instance Model GroupKPI where
  type TableName GroupKPI = "GroupKPI"
  modelInfo = mkModelInfo GroupKPI ident
  modelView = \case
    "kpi" -> Just $ stripId defaultView
    _     -> Nothing
