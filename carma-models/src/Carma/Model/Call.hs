module Carma.Model.Call where

import Data.Aeson as Aeson
import Data.Text
import Data.Typeable
import Data.Model as Model
import Data.Model.Types ((:@))
import Data.Model.View

import Carma.Model.Types           (Coords)
import Carma.Model.Program         (Program)
import Carma.Model.SubProgram.Type (SubProgram)
import Carma.Model.CarMake         (CarMake)
import Carma.Model.CarModel        (CarModel)
import Carma.Model.Usermeta        (Usermeta)
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
    , ("callTaker", one callTaker)
    , ("program", one program)
    , ("wazzup", one wazzup)
    , ("callType", one callType)
    , ("caller", fuzzy $ matchAny
        [one callerName_name, one callerName_ownerName])
    ]

instance Model Call where
  type TableName Call = "calltbl"
  modelInfo = mkModelInfo Call ident
  modelView = \case
    "search" -> Just $ modifyView (searchView callSearchParams) dicts
    _        -> Just $ modifyView defaultView (metas ++ dicts)

dicts :: [(Text, FieldView -> FieldView) :@ Call]
dicts =
  [ dict callType $ (dictOpt "CallTypes")
    {dictParent = Just $ Model.fieldName callerType}
  , dict carModel $ (dictOpt "CarModel")
    {dictParent = Just $ Model.fieldName carMake, dictBounded = True}
  , setMeta "dictionaryLabel" (Aeson.String "realName") callTaker
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

data Call = Call
  { ident :: PK Int Call "Номер звонка"
  , callDate
    :: F LegacyDatetime "callDate" "Дата звонка"
  , endDate
    :: F (Maybe LegacyDatetime) "endDate"  "Время окончания звонка"
  , callTaker
    :: F (IdentI Usermeta) "callTaker" "Сотрудник РАМК"
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
    :: F (Maybe Coords) "coords" "Координаты места поломки"
  , address
    :: F (Maybe Text) "address" "Адрес места поломки"
  , carMake
    :: F (Maybe (IdentI CarMake)) "carMake" "Марка"
  , carModel
    :: F (Maybe (IdentI CarModel)) "carModel" "Модель"
  , callType
    :: F (Maybe (IdentT CallTypes)) "callType" "Тип звонка"
  } deriving Typeable
