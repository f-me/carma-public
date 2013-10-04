
module Carma.Model.Case where

import Data.Text
import Data.Model
import Data.Typeable
import Data.Vector (Vector)

import Data.Time.Calendar (Day)
import Data.Time.Clock    (UTCTime)

import Data.Model.View
import Carma.Model.Types
import Carma.Model.Program  (Program)
import Carma.Model.CarMake  (CarMake)
import Carma.Model.CarModel (CarModel)
import Carma.Model.Wazzup   (Wazzup)
import Carma.Model.City     (City)


data Case = Case
  { callDate
    :: F UTCTime "callDate" "Дата звонка"
  , vwcreatedate
    :: F UTCTime "vwcreatedate" "Дата звонка"
  , callTaker
    :: F Text "callTaker" "Сотрудник РАМК"
  , comment
    :: F (Ident Wazzup) "comment" "Что случилось"
    -- , diagnosis1
    --   :: F (Ident Diagnosis1) "diagnosis1" "Система"
    -- , diagnosis2
    --   :: F (Ident Diagnosis2) "diagnosis2" "Узел/деталь"
    -- , diagnosis3
    --   :: F (Ident Diagnosis3) "diagnosis3" "Описание причины неисправности"
    -- , diagnosis4
    --   :: F (Ident Diagnosis4) "diagnosis4" "Рекомендация"
  , carContact_name
    :: F Text "carContact_name" "Звонящий"
  , carContact_phone1
    :: F Text "carContact_phone1" "Контактные телефоны"
  , carContact_phone2
    :: F Text "carContact_phone2" ""
  , carContact_phone3
    :: F Text "carContact_phone3" ""
  , carContact_phone4
    :: F Text "carContact_phone4" ""
  , carContact_email
    :: F Text "carContact_email" "Email"
  , carContact_contactOwner
    :: F Bool "carContact_contactOwner" "Звонящий владелец?"
  , carContact_ownerName
    :: F Text "carContact_ownerName" "Владелец"
  , carContact_ownerPhone1
    :: F Text "carContact_ownerPhone1" "Контактные телефоны"
  , carContact_ownerPhone2
    :: F Text "carContact_ownerPhone2" ""
  , carContact_ownerPhone3
    :: F Text "carContact_ownerPhone3" ""
  , carContact_ownerPhone4
    :: F Text "carContact_ownerPhone4" ""
  , carContact_ownerEmail
    :: F Text "carContact_ownerEmail" "Email"
  , program
    :: F (Ident Program) "program" "Программа"
    --  , car_vin
    --    :: F Text "car_vin" "VIN"
  , car_make
    :: F (Ident CarMake) "car_make" "Марка"
  , car_model
    :: F (Ident CarModel) "car_model" "Модель"
    --  , car_seller
    --    :: F Text "car_seller" "Дилер, продавший автомобиль"
    --  , car_plateNum
    --    :: F Text "car_plateNum" "Госномер"
    --  , car_makeYear
    --    :: F Text "car_makeYear" "Год производства автомобиля"
    -- , car_color
    --   :: F (Ident Colors) "car_color" "Цвет"
  , car_buyDate
    :: F Day "car_buyDate" "Дата покупки"
  , car_checkupDate
    :: F Day "car_checkupDate" "Дата последнего ТО"
    --  , car_dealerTO
    --    :: F Text "car_dealerTO" "Дилер у которого проходило последнее ТО"
    --  , car_mileage
    --    :: F Text "car_mileage" "Текущий пробег"
    --  , car_checkupMileage
    --    :: F Text "car_checkupMileage" "Пробег на последнем ТО"
  , car_warrantyStart
    :: F Day "car_warrantyStart" "Дата начала действия программы"
  , car_warrantyEnd
    :: F Day "car_warrantyEnd" "Дата окончания действия программы"
    -- , car_contractType
    --   :: F (Ident ContractType) "car_contractType" "Тип контракта"
    -- , car_transmission
    --   :: F (Ident Transmission) "car_transmission" "Коробка передач"
    -- , car_engine
    --   :: F (Ident EngineType) "car_engine" "Тип двигателя"
    --  , car_liters
    --    :: F Text "car_liters" "Объём двигателя"
    --  , car_capacity
    --    :: F Text "car_capacity" "Вместимость"
    --  , car_dims
    --    :: F Text "car_dims" "Габариты"
    --  , car_weight
    --    :: F Text "car_weight" "Масса"
    --  , car_checkPeriod
    --    :: F Text "car_checkPeriod" "Межсервисный интервал"
    -- , car_class
    --   :: F (Ident CarClasses) "car_class" "Класс автомобиля"
    --  , car_makeCode
    --    :: F Text "car_makeCode" "Код марки автомобиля"
    --  , car_modelCode
    --    :: F Text "car_modelCode" "Код модели автомобиля"
    --  , car_faultCode
    --    :: F Text "car_faultCode" "Код неиправности автомобиля"
    --  , cardNumber_cardNumber
    --    :: F Text "cardNumber_cardNumber" "Номер карты участника"
  , cardNumber_validFrom
    :: F Day "cardNumber_validFrom" "Дата регистрации в программе"
  , cardNumber_validUntil
    :: F Day "cardNumber_validUntil" "Программа действует до (дата)"
    --  , cardNumber_validUntilMilage
    --    :: F Text "cardNumber_validUntilMilage" "Программа действует до (пробег)"
    --  , cardNumber_milageTO
    --    :: F Text "cardNumber_milageTO" "Пробег при регистрации в программе"
    --  , cardNumber_serviceInterval
    --    :: F Text "cardNumber_serviceInterval" "Межсервисный интервал"
    --  , cardNumber_cardOwner
    --    :: F Text "cardNumber_cardOwner" "ФИО владельца карты"
    --  , cardNumber_manager
    --    :: F Text "cardNumber_manager" "ФИО менеджера"
    -- , vinChecked
    --   :: F (Ident VINChecked) "vinChecked" "Участие в программе"
  , city
    :: F (Ident City) "city" "Город"
    --  , address_address
    --    :: F Text "address_address" "Адрес"
    --  , address_comment
    --    :: F Text "address_comment" "Примечания"
    --  , address_coords
    --    :: F Text "address_coords" "Координаты"
    --  , address_map
    --    :: F Text "address_map" ""
    --  , temperature
    --    :: F Text "temperature" "Температура"
  , repair
    :: F UTCTime "repair" "Дата починки"
    --  , accord
    --    :: F Text "accord" "Номер согласования"
    --  , dealerCause
    --    :: F Text "dealerCause" "Неисправность со слов дилера/партнёра"
    -- , caseStatus
    --   :: F (Ident CaseStatuses) "caseStatus" "Статус кейса"
  , psaExportNeeded
    :: F Bool "psaExportNeeded" "Требуется выгрузка в PSA"
  , psaExported
    :: F Bool "psaExported" "Выгружен в PSA"
    --  , claim
    --    :: F Text "claim" "Претензия / Благодарность"
    --  , services
    --    :: F Text "services" "Услуги"
    --  , actions
    --    :: F Text "actions" "Действия"
    --  , comments
    --    :: F Text "comments" ""
    --  , files
    --    :: F Text "files" "Прикрепленные файлы"
  } deriving Typeable

instance Model Case where
  type TableName Case = "casetbl"
  modelInfo   = mkModelInfo Case
  modelView _ = defaultView
