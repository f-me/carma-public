
{-# LANGUAGE QuasiQuotes #-}

module Snaplet.SiteConfig.FakeModels
  (newCase
  ,newSvc
  ) where

import Data.Aeson as Aeson
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Snap
import Snaplet.SiteConfig.Config
import Snaplet.SiteConfig.Models


type ModelConfig = Map ByteString (Text, Bool, Bool, Text)


modelConfig :: Text -> Handler b (SiteConfig b) ModelConfig
modelConfig pgm = do
  pg    <- gets pg_search
  let q = fromString [sql|
      select c.field, c.label, c.w, c.required, c.info
        from "NewCaseField" c, programtbl p
        where p.id = c.program and c.r and p.value = ?
      |]
  let mkMap xs = Map.fromList [(f,(l,w,rq,inf)) | (f,l,w,rq,inf) <- xs]
  mkMap <$> liftIO (withResource pg $ \c -> query c q [pgm])


filterFields :: ModelConfig -> [Field] -> [Field]
filterFields cfg = catMaybes . map tr
  where
    tr f
      = Map.lookup (name f) cfg
      >>= \(l, w, rq, inf) -> return
        $ f {meta
            = Map.insert "label" (Aeson.String l)
            . Map.insert "required" (Aeson.Bool rq)
            . Map.insert "infoText" (Aeson.String inf)
            <$> meta f
          , canWrite = w}


newSvc :: Text -> B.ByteString -> Handler b (SiteConfig b) Model
newSvc pgm name = do
  cfg <- modelConfig pgm
  return $ Model
    { modelName    = name
    , title
        = T.encodeUtf8 $ Map.findWithDefault "Неизвестная услуга" name svcNames
    , fields       = filterFields cfg $ newSvcFields name
    , applications = []
    , _canCreateM  = Everyone
    , _canReadM    = Everyone
    , _canUpdateM  = Everyone
    , _canDeleteM  = Everyone
    }


svcNames :: Map.Map B.ByteString Text
svcNames = Map.fromList
  [("averageCommissioner","Аварийный комиссар")
  ,("bank","Банковская поддержка")
  ,("consultation","Консультация")
  ,("continue","Продолжение путешествия")
  ,("deliverCar","Доставка ТС")
  ,("deliverClient","Доставка клиента к отремонтированному автомобилю")
  ,("deliverParts","Доставка запчастей")
  ,("hotel","Гостиница")
  ,("information","Информирование о происшествии")
  ,("insurance","Сбор справок для страховой компании")
  ,("ken","Юридическая помощь")
  ,("rent","Подменный автомобиль")
  ,("sober","Трезвый водитель")
  ,("taxi","Такси")
  ,("tech1","ТО")
  ,("tech","Техпомощь")
  ,("tickets","Заказ билетов")
  ,("towage","Эвакуация")
  ,("transportation","Транспортировка")
  ]

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
      ,mkF "towDealer_partnerId" "text" ""
        [("invisible", Aeson.Bool True)
        ]
      ,mkF "towDealer_address" "text" ""
        [("invisible", Aeson.Bool True)
        ]
      ,mkF "towDealer_coords" "text" ""
        [("invisible", Aeson.Bool True)
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

newCase :: Text -> Handler b (SiteConfig b) Model
newCase pgm = do
  cfg <- modelConfig pgm
  return $ Model
    { modelName    = "case"
    , title        = T.encodeUtf8 "Новый кейс"
    , fields       = filterFields cfg $ newCaseFields
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
    ,("infoText",       "comment")
    ]
  ,mkF "diagnosis1" "dictionary" "Система"
    [("dictionaryName", "Diagnosis1")
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
    [("dictionaryName", "casePrograms")
    ,("dictionaryType", "ComputedDict")
    ,("bounded", Aeson.Bool True)
    ,("targetCategory", "program")
    ,("infoText", "program")
    ]
  ,mkF "car_vin" "dictionary" "VIN"
    [("regexp", "vin")
    ,("transform", "uppercase")
    ,("dictionaryType", "VinDict")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_make" "dictionary" "Марка"
    [("bounded", Aeson.Bool True)
    ,("dictionaryName", "CarMakers")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_model" "dictionary" "Модель"
    [("bounded", Aeson.Bool True)
    ,("dictionaryParent", "car_make")
    ,("dictionaryName", "CarModels")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_seller" "dictionary" "Дилер, продавший автомобиль"
    [("bounded", Aeson.Bool True)
    ,("dictionaryType", "DealersDict")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_buyDate" "date" "Дата покупки"
    [("regexp", "date")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_dealerTO" "dictionary" "Дилер у которого проходило последнее ТО"
    [("bounded", Aeson.Bool True)
    ,("dictionaryType", "DealersDict")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_mileage" "text" "Текущий пробег"
    [("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_checkupMileage" "text" "Пробег на последнем ТО"
    [("mainOnly", Aeson.Bool True)
    ]
  ,mkF "vinChecked" "dictionary" "Участие в программе"
    [("bounded", Aeson.Bool True)
    ,("infoText", "vinChecked")
    ,("dictionaryName", "VINChecked")
    ]
  ,mkF "car_plateNum" "text" "Госномер"
    [("regexp", "plateNum")
    ,("transform", "uppercase")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "cardNumber_cardNumber" "dictionary" "Номер карты участника"
    [("mainOnly", Aeson.Bool True)
    ,("dictionaryType", "CardsDict")
    ]
  ,mkF "car_makeYear" "text" "Год производства автомобиля"
    [("mainOnly", Aeson.Bool True)]
  ,mkF "car_color" "dictionary" "Цвет"
    [("bounded", Aeson.Bool True)
    ,("dictionaryName", "Colors")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "car_transmission" "dictionary" "Коробка передач"
    [("widget", "radio")
    ,("dictionaryName", "Transmission")
    ,("mainOnly", Aeson.Bool True)
    ]
  ,mkF "city" "dictionary" "Город"
    [("bounded", Aeson.Bool True)
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
    [("bounded", Aeson.Bool True)
    ,("dictionaryName", "CaseStatuses")
    ]
  ,mkF "services" "reference" "Услуги"
    [("invisible", Aeson.Bool True)]
  ,mkF "comments" "json" "Комментарии"
    [("invisible", Aeson.Bool True)]
  ]


mkF
  :: ByteString -> ByteString -> Text -> [(ByteString, Value)]
  -> Field
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
