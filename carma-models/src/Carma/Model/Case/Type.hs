module Carma.Model.Case.Type where

import Data.Text
import Data.Typeable

import Data.Time.Calendar (Day)
import Data.Time.Clock    (UTCTime)

import Data.Model

import Carma.Model.Program  (Program)
import Carma.Model.CarMake  (CarMake)
import Carma.Model.CarModel (CarModel)
import Carma.Model.Wazzup   (Wazzup)
import Carma.Model.City     (City)


data Case = Case
  { callDate
    :: F Day "callDate" "Дата звонка" -- FIXME: it's UTCTime actually
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
  , contact_name
    :: F Text "contact_name" "Звонящий"
  , contact_phone1
    :: F Text "contact_phone1" "Контактные телефоны"
  , contact_phone2
    :: F Text "contact_phone2" ""
  , contact_phone3
    :: F Text "contact_phone3" ""
  , contact_phone4
    :: F Text "contact_phone4" ""
  , contact_email
    :: F Text "contact_email" "Email"
  , contact_contactOwner
    :: F Bool "contact_contactOwner" "Звонящий владелец?"
  , contact_ownerName
    :: F Text "contact_ownerName" "Владелец"
  , contact_ownerPhone1
    :: F Text "contact_ownerPhone1" "Контактные телефоны"
  , contact_ownerPhone2
    :: F Text "contact_ownerPhone2" ""
  , contact_ownerPhone3
    :: F Text "contact_ownerPhone3" ""
  , contact_ownerPhone4
    :: F Text "contact_ownerPhone4" ""
  , contact_ownerEmail
    :: F Text "contact_ownerEmail" "Email"
  , program
    :: F (Ident Program) "program" "Программа"
  , car_vin
    :: F Text "car_vin" "VIN"
  , car_make
    :: F (Ident CarMake) "car_make" "Марка"
  , car_model
    :: F (Ident CarModel) "car_model" "Модель"
    --  , car_seller
    --    :: F Text "car_seller" "Дилер, продавший автомобиль"
  , car_plateNum
    :: F Text "car_plateNum" "Госномер"
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
  , cardNumber_cardNumber
    :: F Text "cardNumber_cardNumber" "Номер карты участника"
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
    --  , caseAddress_address
    --    :: F Text "caseAddress_address" "Адрес"
    --  , caseAddress_comment
    --    :: F Text "caseAddress_comment" "Примечания"
    --  , caseAddress_coords
    --    :: F Text "caseAddress_coords" "Координаты"
    --  , caseAddress_map
    --    :: F Text "caseAddress_map" ""
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
