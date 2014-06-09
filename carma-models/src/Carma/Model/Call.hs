module Carma.Model.Call where

import Data.Aeson as Aeson
import Data.Text
import Data.Typeable
import Data.Model as Model
import Data.Model.Types ((:@))
import Data.Model.View

import Carma.Model.Program         (Program)
import Carma.Model.SubProgram.Type (SubProgram)
import Carma.Model.LegacyTypes

import Carma.Model.Diagnostics.Wazzup (Wazzup)

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
    , ("callTaker", fuzzy $ one callTaker)
    , ("program", one program)
    , ("wazzup", one wazzup)
    , ("callType", one callType)
    , ("caller", fuzzy $ matchAny
        [one callerName_name, one callerName_ownerName])
    ]

instance Model Call where
  type TableName Call = "calltbl"
  modelInfo = mkModelInfo Call ident `withLegacyName` "call"
  modelView = \case
    "search" -> Just $ modifyView (searchView callSearchParams) dicts
    _        -> Just $ modifyView defaultView (metas ++ dicts ++ callOldCRUDHacks)

dicts :: [(Text, FieldView -> FieldView) :@ Call]
dicts =
  [ dict callType $ (dictOpt "CallTypes")
    {dictParent = Just $ Model.fieldName callerType}
  , dict carModel $ (dictOpt "CarModels")
    {dictParent = Just $ Model.fieldName carMake, dictBounded = True}
  , dict callTaker $ (dictOpt "users")
  , setMeta "dictionaryParent"
    (Aeson.String $ Model.fieldName program) subprogram
  ]

metas :: [(Text, FieldView -> FieldView) :@ Call]
metas =
    [ readonly callDate
    , readonly endDate

    , readonly callTaker
    , required callTaker

    , invisible coords
    , invisible address
    ]

-- | Mark several new-style dictionaries to use dictionaryStringify,
-- to wrap integers in strings to be compatible with the old CRUD.
callOldCRUDHacks :: [(Text, FieldView -> FieldView) :@ Call]
callOldCRUDHacks =
    [ setMeta "dictionaryStringify" (Aeson.Bool True) program
    , setMeta "dictionaryStringify" (Aeson.Bool True) subprogram
    , setMeta "dictionaryStringify" (Aeson.Bool True) wazzup
    ]

data Call = Call
  { ident :: PK Int Call "Номер звонка"
  , callDate
    :: F LegacyDatetime "callDate" "Дата звонка"
  , endDate
    :: F (Maybe LegacyDatetime) "endDate"  "Время окончания звонка"
  , callTaker
    :: F (IdentT Users) "callTaker" "Сотрудник РАМК"
  , program
    :: F (Maybe (IdentI Program)) "program" "Программа"
  , subprogram
    :: F (Maybe (IdentI SubProgram)) "subprogram" "Подпрограмма"
  , wazzup
    :: F (Maybe (IdentI Wazzup)) "wazzup" "Что случилось"
  , customerComment
    :: F (Maybe Text) "customerComment" "Неисправность со слов клиента"
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
  , callerType
    :: F (Maybe (IdentT CallerTypes)) "callerType" "Кто звонит?"
  , city
    :: F (Maybe (IdentT DealerCities)) "city" "Город"
  , coords
    :: F (Maybe Text) "coords" "Координаты места поломки"
  , address
    :: F (Maybe Text) "address" "Адрес места поломки"
  , carMake
    :: F (Maybe (IdentT CarMakers)) "carMake" "Марка"
  , carModel
    :: F (Maybe (IdentT CarModels)) "carModel" "Модель"
  , callType
    :: F (Maybe (IdentT CallTypes)) "callType" "Тип звонка"
  } deriving Typeable
