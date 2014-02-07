module Carma.Model.Call where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Program  (Program)
import Carma.Model.Wazzup   (Wazzup)
import Carma.Model.LegacyTypes

import Carma.Model.Search as S

callSearchParams :: [(Text, [Predicate Call])]
callSearchParams
  = [ ("id", one ident)
    , ("callDate", interval callDate)
    ]

instance Model Call where
  type TableName Call = "casetbl"
  modelInfo   = mkModelInfo Call ident
  modelView "search" = modifyView (searchView callSearchParams)
                       [modifyByName "id" (\v -> v { fv_type = "ident" })]
  modelView _ = defaultView

data Call = Call
  { ident :: PK Int Call "Номер звонка"
  , callDate
   :: F LegacyDatetime "callDate" "Дата звонка"
  , callTaker
   :: F Text "callTaker" "Сотрудник РАМК"
  , program
    :: F (Maybe (IdentT Program)) "program" "Программа"
  , wazzup
    :: F (Maybe (Ident Text Wazzup)) "wazzup" "Что случилось"
  , carContact_name
    :: F (Maybe Text) "carContact_name" "Звонящий"
  , carContact_phone1
    :: F (Maybe Text) "carContact_phone1" "Контактные телефоны"
  , carContact_phone2
    :: F (Maybe Text) "carContact_phone2" ""
  , carContact_phone3
    :: F (Maybe Text) "carContact_phone3" ""
  , carContact_phone4
    :: F (Maybe Text) "carContact_phone4" ""
  , carContact_email
    :: F (Maybe Text) "carContact_email" "Email"
  , carContact_contactOwner
    :: F (Maybe Bool) "carContact_contactOwner" "Звонящий владелец?"
  , carContact_ownerName
    :: F (Maybe Text) "carContact_ownerName" "Владелец"
  , carContact_ownerPhone1
    :: F (Maybe Text) "carContact_ownerPhone1" "Контактные телефоны"
  , carContact_ownerPhone2
    :: F (Maybe Text) "carContact_ownerPhone2" ""
  , carContact_ownerPhone3
    :: F (Maybe Text) "carContact_ownerPhone3" ""
  , carContact_ownerPhone4
    :: F (Maybe Text) "carContact_ownerPhone4" ""
  , carContact_ownerEmail
    :: F (Maybe Text) "carContact_ownerEmail" "Email"
  -- , callerType
  --   :: F (Maybe (Ident CallerTypes)) "callerType" "Кто звонит?"
  , city
    :: F (Maybe (IdentT DealerCities)) "city" "Город"
--  , coords
--    :: F (Maybe Text) "coords" "Координаты места поломки"
--  , address
--    :: F (Maybe Text) "address" "Адрес места поломки"
  , carMake
    :: F (Maybe (IdentT CarMakers)) "carMake" "Марка"
  , carModel
    :: F (Maybe (IdentT CarModels)) "carModel" "Модель"
  -- , callType
  --   :: F (Maybe (Ident CallTypes)) "callType" "Тип звонка"
  } deriving Typeable
