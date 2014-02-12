module Carma.Model.Case.Type where

import Data.Text
import Data.Typeable

import Data.Model
import Carma.Model.Program  (Program)
import Carma.Model.Wazzup   (Wazzup)
import Carma.Model.LegacyTypes


data Case = Case
  { ident :: PK Int Case "Номер кейса"
  , callDate
    :: F LegacyDatetime "callDate" "Дата звонка"
  , vwcreatedate
    :: F (Maybe LegacyDatetime) "vwcreatedate" "Дата звонка"
  , callTaker
    :: F Text            "callTaker" "Сотрудник РАМК"
  , comment
    :: F (Maybe (Ident Text Wazzup)) "comment" "Что случилось"
  , diagnosis1
    :: F (Maybe (IdentT Diagnosis1)) "diagnosis1" "Система"
  , diagnosis2
    :: F (Maybe (IdentT Diagnosis2)) "diagnosis2" "Узел/деталь"
  , diagnosis3
    :: F (Maybe (IdentT Diagnosis3)) "diagnosis3" "Описание причины неисправности"
  , diagnosis4
    :: F (Maybe (IdentT Diagnosis4)) "diagnosis4" "Рекомендация"
  , contact_name
    :: F (Maybe Text) "contact_name" "Звонящий"
  , contact_phone1
    :: F (Maybe Phone) "contact_phone1" "Контактные телефоны"
  , contact_phone2
    :: F (Maybe Phone) "contact_phone2" ""
  , contact_phone3
    :: F (Maybe Phone) "contact_phone3" ""
  , contact_phone4
    :: F (Maybe Phone) "contact_phone4" ""
  , contact_email
    :: F (Maybe Text) "contact_email" "Email"
  , contact_contactOwner
    :: F (Maybe Checkbox)     "contact_contactOwner" "Звонящий владелец?"
  , contact_ownerName
    :: F (Maybe Text) "contact_ownerName" "Владелец"
  , contact_ownerPhone1
    :: F (Maybe Phone) "contact_ownerPhone1" "Контактные телефоны"
  , contact_ownerPhone2
    :: F (Maybe Phone) "contact_ownerPhone2" ""
  , contact_ownerPhone3
    :: F (Maybe Phone) "contact_ownerPhone3" ""
  , contact_ownerPhone4
    :: F (Maybe Phone) "contact_ownerPhone4" ""
  , contact_ownerEmail
    :: F (Maybe Text) "contact_ownerEmail" "Email"
  , program
    :: F (IdentT Program)  "program" "Программа"

  , car_vin
    :: F (Maybe Text) "car_vin" "VIN"
  , car_make
    :: F (Maybe (IdentT CarMakers)) "car_make" "Марка"
  , car_model
    :: F (Maybe (IdentT CarModels)) "car_model" "Модель"
  , car_seller
    :: F (Maybe (IdentT Partner)) "car_seller" "Дилер, продавший автомобиль"
  , car_plateNum
    :: F (Maybe Text) "car_plateNum" "Госномер"
  , car_makeYear
      :: F (Maybe Int) "car_makeYear" "Год производства автомобиля"
  , car_color
     :: F (Maybe (IdentT Colors)) "car_color" "Цвет"
  , car_buyDate
    :: F (Maybe LegacyDate) "car_buyDate" "Дата покупки"
  , car_checkupDate
    :: F (Maybe LegacyDate) "car_checkupDate" "Дата последнего ТО"
  , car_dealerTO
    :: F (Maybe (IdentT Partner)) "car_dealerTO" "Дилер у которого проходило последнее ТО"
  , car_mileage
    :: F (Maybe Int) "car_mileage" "Текущий пробег"
  , car_checkupMileage
    :: F (Maybe Int) "car_checkupMileage" "Пробег на последнем ТО"
  , car_warrantyStart
    :: F (Maybe LegacyDate) "car_warrantyStart" "Дата начала действия программы"
  , car_warrantyEnd
    :: F (Maybe LegacyDate) "car_warrantyEnd" "Дата окончания действия программы"
  , car_contractType
    :: F (Maybe (IdentT ContractType)) "car_contractType" "Тип контракта"
  , car_transmission
    :: F (Maybe (IdentT Transmission)) "car_transmission" "Коробка передач"
  , car_engine
    :: F (Maybe (IdentT EngineType)) "car_engine" "Тип двигателя"
  , car_liters
    :: F (Maybe Text) "car_liters" "Объём двигателя"
  , car_capacity
    :: F (Maybe Text) "car_capacity" "Вместимость"
  , car_dims
    :: F (Maybe Text) "car_dims" "Габариты"
  , car_weight
    :: F (Maybe Text) "car_weight" "Масса"
  , car_checkPeriod
     :: F (Maybe Int) "car_checkPeriod" "Межсервисный интервал"
  , car_class
    :: F (Maybe (IdentT CarClasses)) "car_class" "Класс автомобиля"
  , car_makeCode
    :: F (Maybe Text) "car_makeCode" "Код марки автомобиля"
  , car_modelCode
    :: F (Maybe Text) "car_modelCode" "Код модели автомобиля"
  , car_faultCode
    :: F (Maybe Text) "car_faultCode" "Код неиправности автомобиля"
  , car_legalForm
    :: F (Maybe (IdentT LegalForms)) "car_legalForm" "Физическое / Юридическое лицо"

  , cardNumber_cardNumber
    :: F (Maybe Text) "cardNumber_cardNumber" "Номер карты участника"
  , cardNumber_validFrom
    :: F (Maybe LegacyDate) "cardNumber_validFrom" "Дата регистрации в программе"
  , cardNumber_validUntil
    :: F (Maybe LegacyDate) "cardNumber_validUntil" "Программа действует до (дата)"
  , cardNumber_validUntilMilage
    :: F (Maybe Int) "cardNumber_validUntilMilage" "Программа действует до (пробег)"
  , cardNumber_milageTO
    :: F (Maybe Int) "cardNumber_milageTO" "Пробег при регистрации в программе"
  , cardNumber_serviceInterval
    :: F (Maybe Int) "cardNumber_serviceInterval" "Межсервисный интервал"
  , cardNumber_cardOwner
    :: F (Maybe Text) "cardNumber_cardOwner" "ФИО владельца карты"
  , cardNumber_managerr
    :: F (Maybe Text) "cardNumber_manager" "ФИО менеджера"
  , vinChecked
    :: F (Maybe (IdentT VINChecked)) "vinChecked" "Участие в программе"
  , city
    :: F (Maybe (IdentT DealerCities)) "city" "Город"
  , caseAddress_address
    :: F (Maybe PickerField) "caseAddress_address" "Адрес места поломки"
  , caseAddress_comment
    :: F (Maybe Text) "caseAddress_comment" "Примечания"
  , caseAddress_coords
    :: F (Maybe PickerField) "caseAddress_coords" "Координаты"
  , caseAddress_map
    :: F (Maybe MapField) "caseAddress_map" ""
  , temperature
    :: F (Maybe Text) "temperature" "Температура"
  , repair
    :: F (Maybe LegacyDate) "repair" "Дата починки"
  , accord
    :: F (Maybe Text) "accord" "Номер согласования"
  , dealerCause
    :: F (Maybe Text) "dealerCause" "Неисправность со слов дилера/партнёра"
  , caseStatus
    :: F (Maybe (IdentT CaseStatuses)) "caseStatus" "Статус кейса"
  , psaExportNeeded
    :: F (Maybe Checkbox) "psaExportNeeded" "Требуется выгрузка в PSA"
  , psaExported
    :: F (Maybe Checkbox) "psaExported" "Выгружен в PSA"
  , claim
    :: F (Maybe Text) "claim" "Претензия / Благодарность"
  , services
    :: F (Maybe Reference) "services" "Услуги"
  , actions
    :: F (Maybe Reference) "actions" "Действия"
  , comments
    :: F (Maybe Json) "comments" ""
  , files
    :: F (Maybe Reference) "files" "Прикрепленные файлы"
  } deriving Typeable
