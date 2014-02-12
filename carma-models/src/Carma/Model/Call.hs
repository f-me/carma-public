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
  = [ ("phone", fuzzy $ matchAny
        [ one callerName_phone1, one callerName_ownerPhone1
        , one callerName_phone2, one callerName_ownerPhone2
        , one callerName_phone3, one callerName_ownerPhone3
        , one callerName_phone4, one callerName_ownerPhone4
        ])
    , ("callDate", interval callDate)
    ]

instance Model Call where
  type TableName Call = "calltbl"
  modelInfo   = mkModelInfo Call ident
  modelView "search" = searchView callSearchParams
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
  , callerName_name
    :: F (Maybe Text) "callerName_name" "Звонящий"
  , callerName_phone1
    :: F (Maybe Text) "callerName_phone1" "Контактные телефоны"
  , callerName_phone2
    :: F (Maybe Text) "callerName_phone2" ""
  , callerName_phone3
    :: F (Maybe Text) "callerName_phone3" ""
  , callerName_phone4
    :: F (Maybe Text) "callerName_phone4" ""
  , callerName_email
    :: F (Maybe Text) "callerName_email" "Email"
  , callerName_contactOwner
    :: F (Maybe Bool) "callerName_contactOwner" "Звонящий владелец?"
  , callerName_ownerName
    :: F (Maybe Text) "callerName_ownerName" "Владелец"
  , callerName_ownerPhone1
    :: F (Maybe Text) "callerName_ownerPhone1" "Контактные телефоны"
  , callerName_ownerPhone2
    :: F (Maybe Text) "callerName_ownerPhone2" ""
  , callerName_ownerPhone3
    :: F (Maybe Text) "callerName_ownerPhone3" ""
  , callerName_ownerPhone4
    :: F (Maybe Text) "callerName_ownerPhone4" ""
  , callerName_ownerEmail
    :: F (Maybe Text) "callerName_ownerEmail" "Email"
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
