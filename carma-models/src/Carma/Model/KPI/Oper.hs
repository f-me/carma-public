module Carma.Model.KPI.Oper (OperKPI(..)) where

import           Data.Typeable

import qualified Data.Aeson as Aeson
import           Data.Time.Clock (DiffTime, UTCTime)

import           Data.Model
import           Data.Model.View

import           Carma.Model.Usermeta (Usermeta)
import           Carma.Model.Case     (Case)
import           Carma.Model.Types    (UserStateVal)


data OperKPI = OperKPI
  { frontIdent   :: PK Int OperKPI     "KPI пользователя"
  , user         :: F (IdentI Usermeta) "userid"          "Оператор"

  , currentCase  :: F (Maybe (IdentI Case))   "currentCase" "Текущий кейс"
  , loginTime    :: F (Maybe UTCTime) "loginTime" "Время входа"
  , currentState :: F UserStateVal         "currentState"  "Текущий статус"
  , lastState    :: F (Maybe UserStateVal) "lastState"  "Последний статус"
  , inCurrent    :: F DiffTime "inCurrent" "Время в текущем"

  , inReady      :: F (Maybe DiffTime) "Ready"   "Готов"
  , inBusy       :: F (Maybe DiffTime) "Busy"    "Занят"
  , inDinner     :: F (Maybe DiffTime) "Dinner"  "Обед"
  , inNA         :: F (Maybe DiffTime) "NA"      "NA"
  , inRest       :: F (Maybe DiffTime) "Rest"    "Перерыв"
  , inServiceBreak
                 :: F (Maybe DiffTime) "ServiceBreak" "Служебный перерыв"
  , inLoggedOut  :: F (Maybe DiffTime) "LoggedOut"    "Разлогинен"

  , totalRest      :: F (Maybe DiffTime) "totalRest"     "Всего в перерывах"
  , totalLoggedIn  :: F (Maybe DiffTime) "totalLoggedIn" "Всего в системе"

} deriving Typeable

instance Model OperKPI where
  type TableName OperKPI = "OperKPI"
  modelInfo = mkModelInfo OperKPI frontIdent
  modelView = \case
    "kpi" -> Just $ modifyView (stripId $ defaultView)
      [ setMeta "dictionaryLabel" (Aeson.String "realName") user
      , invisible lastState
      , widget "case-ident" currentCase
      ]
    _     -> Nothing
