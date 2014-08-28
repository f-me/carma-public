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

  , infoTime     :: F (Maybe DiffTime) "infoTime"  "В разговоре: информационные"
  , infoCount    :: F (Maybe Int)      "infoCount" "Количество: информационные"

  , procTime  :: F (Maybe DiffTime) "procTime"  "В разговоре: Обработка кейса"
  , procCount :: F (Maybe Int)      "procCount" "Количество: Обработка кейса"

  , newTime  :: F (Maybe DiffTime) "newTime"  "В разговоре: Создание кейса"
  , newCount :: F (Maybe Int)      "newCount" "Количество: Создание кейса"

  -- , callTime :: F DiffTime "callTime" "Итого: Время в разговоре"
  -- , amount   :: F Int      "amount"   "Итого: Количество звонков"
  -- , avgTime  :: F DiffTime "avgTime"  "Итого: Количество звонков"
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
} deriving Typeable

instance Model StatKPI where
  type TableName StatKPI = "StatKPI"
  modelInfo = mkModelInfo StatKPI frontIdent
  modelView = \case
    "kpi" -> Just $ modifyView (stripId $ defaultView)
      [setMeta "dictionaryLabel" (Aeson.String "realName") user]
    _     -> Nothing

