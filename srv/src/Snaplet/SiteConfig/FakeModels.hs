
module Snaplet.SiteConfig.FakeModels where

import Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

import Snaplet.SiteConfig.Models


newSvc :: B.ByteString -> Model
newSvc name = Model
  { modelName    = name
  , title        = T.encodeUtf8 "Новая услуга"
  , fields       = newSvcFields name
  , applications = []
  , _canCreateM  = Everyone
  , _canReadM    = Everyone
  , _canUpdateM  = Everyone
  , _canDeleteM  = Everyone
  }

newSvcFields :: B.ByteString -> [Field]
newSvcFields name
  = [mkF "parentId" "text" "Ссылка на кейс"
      [("invisible", Aeson.Bool True)
      ]
    ,mkF "times_expectedServiceStart" "datetime" "Ожидаемое время начала оказания услуги"
      [("regexp", "datetime")
      ,("infoText", "datetime")
      ,("mainOnly", Aeson.Bool True)
      ]
    ,mkF "times_expectedServiceEnd" "datetime" "Ожидаемое время окончания оказания услуги"
      [("regexp", "datetime")
      ,("infoText", "datetime")
      ,("mainOnly", Aeson.Bool True)
      ]
    ,mkF "times_expectedDispatch" "datetime" "Время выезда партнёра"
      [("regexp", "datetime")
      ,("infoText", "datetime")
      ,("mainOnly", Aeson.Bool True)
      ]
    ,mkF "payType" "dictionary" "Тип оплаты"
      [("dictionaryName", "PaymentTypes")
      ,("bounded",       Aeson.Bool True)
      ]
    ]
  ++ case name of
    "towage" ->
      [mkF "towDealer_partner" "text" "Дилер (куда эвакуируют автомобиль)"
        [("widget", "partner")
        ,("mainOnly", Aeson.Bool True)
        ]
      ,mkF "towerType" "dictionary" "Тип эвакуатора"
        [("dictionaryName", "TowerTypes")
        ,("bounded",       Aeson.Bool True)
        ]
      ,mkF "towType" "dictionary" "Вид эвакуации"
        [("dictionaryName", "TowTypes")
        ,("bounded",       Aeson.Bool True)
        ]
      ,mkF "accident" "checkbox" "ДТП"
        []
      ,mkF "vandalism" "checkbox" "Случай вандализма"
        []
      ,mkF "canNeutral" "checkbox" "Переключается на нейтральную передачу"
        []
      ,mkF "towingPointPresent" "checkbox" "Есть буксировочный крюк"
        []
      ,mkF "manipulatorPossible" "checkbox" "Есть место для манипулятора"
        []
      ,mkF "companion" "checkbox" "Клиент/Доверенное лицо будет сопровождать автомобиль"
        []
      ]
    "tech" ->
      [mkF "techType" "dictionary" "Услуга"
        [("dictionaryName", "TechTypes")
        ,("bounded",       Aeson.Bool True)
        ,("required",      Aeson.Bool True)
        ]
      ]
    "taxi" ->
      [mkF "taxiFrom_address" "picker" "Где забрать"
        [("picker", "geoPicker")
        ,("mainOnly", Aeson.Bool True)
        ]
      ]
    _ -> []
  ++[mkF "urgentService" "dictionary" "Приоритетная услуга"
      [("dictionaryName", "UrgentServiceReason")
      ,("bounded",       Aeson.Bool False)
      ]
    ,mkF "status" "dictionary" "Статус услуги"
      [("dictionaryName", "ServiceStatuses")
      ,("bounded",       Aeson.Bool True)
      ]
    ]

newCase :: Model
newCase = Model
  { modelName    = "case"
  , title        = T.encodeUtf8 "Новый кейс"
  , fields       = newCaseFields
  , applications = []
  , _canCreateM  = Everyone
  , _canReadM    = Everyone
  , _canUpdateM  = Everyone
  , _canDeleteM  = Everyone
  }


newCaseFields :: [Field]
newCaseFields =
  [mkF "comment" "dictionary" "Что случилось"
    [("dictionaryName", "Wazzup")
    ,("required",       Aeson.Bool True)
    ,("infoText",       "comment")
    ]
  ,mkF "diagnosis1" "dictionary" "Система"
    [("dictionaryName", "Diagnosis1")
    ,("required",       Aeson.Bool True)
    ,("infoText",       "system")
    ]
  ,mkF "diagnosis2" "dictionary" "Узел/деталь"
    [("dictionaryName",   "Diagnosis2")
    ,("dictionaryParent", "diagnosis1")
    ,("infoText",         "detail")
    ]
  ,mkF "diagnosis3" "dictionary" "Описание причины неисправности"
    [("dictionaryName", "Diagnosis3")
    ,("infoText",       "diagnosis3")
    ]
  ,mkF "contact_name" "text" "Звонящий"
    [("transform", "capitalize")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "contact_phone1" "phone" "Контактный телефон"
    [("regexp", "phone")
    ,("picker", "callPlease")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "contact_contactOwner" "checkbox" "Звонящий владелец?"
    [("infoText", "owner")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "contact_ownerName" "text" "Владелец"
    [("infoText", "capitalize")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "contact_ownerPhone1" "phone" "Контактный телефон владельца"
    [("regexp", "phone")
    ,("picker", "callPlease")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "program" "dictionary" "Программа"
    [("dictionaryName", "Programs")
    ,("required", Aeson.Bool True)
    ,("bounded", Aeson.Bool True)
    ,("targetCategory", "program")
    ,("infoText", "program")
    ]
  ,mkF "car_vin" "dictionary" "VIN"
    [("required", Aeson.Bool True)
    ,("regexp", "vin")
    ,("transform", "uppercase")
    ,("dictionaryType", "VinDict")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_make" "dictionary" "Марка"
    [("required", Aeson.Bool True)
    ,("bounded", Aeson.Bool True)
    ,("dictionaryName", "CarMakers")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_model" "dictionary" "Модель"
    [("required", Aeson.Bool True)
    ,("bounded", Aeson.Bool True)
    ,("dictionaryParent", "car_make")
    ,("dictionaryName", "CarModels")
    ,("dictionaryParent", "car_make")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_seller" "dictionary" "Дилер, продавший автомобиль"
    [("required", Aeson.Bool True)
    ,("bounded", Aeson.Bool True)
    ,("dictionaryType", "DealersDict")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_buyDate" "date" "Дата покупки"
    [("required", Aeson.Bool True)
    ,("regexp", "date")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_dealerTO" "dictionary" "Дилер у которого проходило последнее ТО"
    [("required", Aeson.Bool True)
    ,("bounded", Aeson.Bool True)
    ,("dictionaryType", "DealersDict")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_mileage" "text" "Текущий пробег"
    [("required", Aeson.Bool True)
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_checkupMileage" "text" "Пробег на последнем ТО"
    [("required", Aeson.Bool True)
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "vinChecked" "dictionary" "Участие в программе"
    [("required", Aeson.Bool True)
    ,("bounded", Aeson.Bool True)
    ,("infoText", "vinChecked")
    ,("dictionaryName", "VINChecked")
    ]
  ,mkF "car_plateNum" "text" "Госномер"
    [("required", Aeson.Bool True)
    ,("regexp", "plateNum")
    ,("transform", "uppercase")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "cardNumber_cardNumber" "text" "Номер карты участника"
    [("mainOnly", Aeson.Bool True)]
  ,mkF "car_makeYear" "text" "Год производства автомобиля"
    [("mainOnly", Aeson.Bool True)]
  ,mkF "car_color" "dictionary" "Цвет"
    [("required", Aeson.Bool True)
    ,("bounded", Aeson.Bool True)
    ,("dictionaryName", "Colors")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_transmission" "dictionary" "Коробка передач"
    [("required", Aeson.Bool True)
    ,("widget", "radio")
    ,("dictionaryName", "Transmission")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "city" "dictionary" "Город"
    [("required", Aeson.Bool True)
    ,("bounded", Aeson.Bool True)
    ,("infoText", "city")
    ,("dictionaryName", "DealerCities")
    ]
  ,mkF "caseAddress_address" "picker" "Адрес места поломки"
    [("picker", "geoPicker")
    ,("targetCoords", "caseAddress_coords")
    ,("targetMap", "caseAddress_map")
    ]
  ,mkF "caseAddress_comment" "text" "Примечания"
    []
  ,mkF "caseAddress_coords" "picker" "Координаты"
    [("picker", "reverseGeoPicker")
    ,("infoText", "coords")
    ,("targetAddr", "caseAddress_address")
    ,("targetMap", "caseAddress_map")
    ]
  ,mkF "caseAddress_map" "map" ""
    [("cityField", "city")
    ,("targetCoords", "caseAddress_coords")
    ,("targetAddr", "caseAddress_address")
    ]
  ,mkF "caseStatus" "dictionary" "Статус кейса"
    [("required", Aeson.Bool True)
    ,("bounded", Aeson.Bool True)
    ,("dictionaryName", "CaseStatuses")
    ]
  ,mkF "services" "reference" "Услуги"
    [("invisible", Aeson.Bool True)]
  ]


mkF nm typ lab mt = Field
  { name      = nm
  , fieldType = typ
  , groupName = Nothing
  , canWrite  = True
  , meta      = Just $ Map.fromList
    $ [("label",    Aeson.String lab)
      ]
    ++ mt
  }
