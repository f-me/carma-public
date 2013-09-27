module Carma.Model.CaseSearch where

import Data.Typeable
import Data.Text (Text)
import Data.Vector (Vector)
import Data.HashMap.Strict (fromList)

import Data.Time.Calendar (Day)

import Data.Model
import Data.Model.View

import Carma.Model.CarMake (CarMake)
import Carma.Model.City    (City)
import Carma.Model.Program (Program)
import Carma.Model.Wazzup  (Wazzup)
import Carma.Model.Types

import Carma.Search.Condition
import Carma.Search.Patch()


data CaseSearch = CaseSearch
  { caseId     :: F Int  "id" "Кейс"
  , vin        :: F Text "car_vin" "VIN"
  , cardNumber :: F Text "cardNumber" "Карта участника"
  , plateNum   :: F Text "car_platenum" "Госномер"
  , phone      :: F Text "phone" "Телефон"
  , program    :: F (Vector (Dict Program)) "program" "Программа"
  , contact    :: F Text "contact" "Контакт имя"
  , owner      :: F Text "contact_ownerName" "Владалец"
  , address    :: F Text "caseAddress_address" "Адрес места поломки"
  , callDate   :: F DayInterval "callDate" "Дата создания кейса"
  , city       :: F (Vector (Dict City))    "city"     "Город"
  , carMake    :: F (Vector (Dict CarMake)) "car_make" "Марка"
  , callTaker  :: F Text "callTaker" "Сотрудник принявший звонок"
  , comment    :: F (Vector (Dict Wazzup)) "comment" "Что случилось"
  } deriving (Typeable)

instance Model CaseSearch where
  type TableName CaseSearch = "casetbl"
  modelFields = getModelFields CaseSearch
  modelView _ = defaultView

caseConditions :: ConditionsHM CaseSearch
caseConditions = fromList
  [ ("id",    [full caseId])
  , ("phone", [fuzzyMany phone ["phone1","phone2","phone3","phone4"]])
  , ("city",  [full city])
  , ("comment", [fuzzy comment])
  , ("callDate", [inInterval callDate])
  ]
