module Carma.Model.Case.Type where

import Data.Text
import Data.Typeable

import Data.Model

import Carma.Model.Types

import Carma.Model.Contract     (Contract)
import Carma.Model.Program      (Program)
import Carma.Model.SubProgram   (SubProgram)
import Carma.Model.Transmission (Transmission)
import Carma.Model.Engine       (Engine)
import Carma.Model.CarClass     (CarClass)
import Carma.Model.Wazzup       (Wazzup)
import Carma.Model.LegacyTypes hiding (CarClasses)


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
    :: F Checkbox     "contact_contactOwner" "Звонящий владелец?"
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
    :: F (Maybe (IdentT Program))  "program" "Программа"
  , subprogram
    :: F (Maybe (IdentI SubProgram))  "subprogram" "Подпрограмма"

  , contractIdentifier
    :: F (Maybe Text) "contractIdentifier" "Идентификатор контракта"
  , contract
    :: F (Maybe (IdentI Contract)) "contract" "Контракт"
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
    :: F (Maybe TInt) "car_makeYear" "Год производства автомобиля"
  , car_color
    :: F (Maybe (IdentT Colors)) "car_color" "Цвет"
  , car_buyDate
    :: F (Maybe LegacyDate) "car_buyDate" "Дата покупки"
  , car_dealerTO
    :: F (Maybe (IdentT Partner)) "car_dealerTO" "Дилер у которого проходило последнее ТО"
  , car_mileage
    :: F (Maybe TInt) "car_mileage" "Текущий пробег"
  , car_transmission
    :: F (Maybe (IdentI Transmission)) "car_transmission" "Коробка передач"
  , car_engine
    :: F (Maybe (IdentI Engine)) "car_engine" "Тип двигателя"
  , car_liters
    :: F (Maybe Text) "car_liters" "Объём двигателя"
  , car_class
    :: F (Maybe (IdentI CarClass)) "car_class" "Класс автомобиля"

  , vinChecked
    :: F (Maybe (IdentT VINChecked)) "vinChecked" "Участие в программе"
  , city
    :: F (Maybe (IdentT DealerCities)) "city" "Город"
  , caseAddress_address
    :: F PickerField "caseAddress_address" "Адрес места поломки"
  , caseAddress_comment
    :: F (Maybe Text) "caseAddress_comment" "Примечания"
  , caseAddress_coords
    :: F PickerField "caseAddress_coords" "Координаты"
  , caseAddress_map
    :: F MapField "caseAddress_map" ""
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
    :: F Checkbox "psaExportNeeded" "Требуется выгрузка в PSA"
  , psaExported
    :: F Checkbox "psaExported" "Выгружен в PSA"
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
