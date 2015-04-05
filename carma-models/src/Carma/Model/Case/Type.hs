module Carma.Model.Case.Type where

import Data.Aeson (Value)
import Data.Text
import Data.Time.Calendar
import Data.Time.Clock
import Data.Typeable

import Data.Model

import Carma.Model.CaseSource
import Carma.Model.CaseStatus
import Carma.Model.City         (City)
import Carma.Model.Contract     (Contract)
import Carma.Model.ContractCheckStatus
import Carma.Model.Partner      (Partner)
import Carma.Model.Program      (Program)
import Carma.Model.SubProgram   (SubProgram)
import Carma.Model.Transmission (Transmission)
import Carma.Model.Engine       (Engine)
import Carma.Model.CarClass     (CarClass)
import Carma.Model.CarMake      (CarMake)
import Carma.Model.CarModel     (CarModel)
import Carma.Model.LegacyTypes

import Carma.Model.Diagnostics.Cause      (Cause)
import Carma.Model.Diagnostics.Part       (Part)
import Carma.Model.Diagnostics.Suggestion (Suggestion)
import Carma.Model.Diagnostics.System     (System)
import Carma.Model.Diagnostics.Wazzup     (Wazzup)

import Carma.Model.Usermeta (Usermeta)

data Case = Case
  { ident :: PK Int Case "Номер кейса"
  , callDate
    :: F (Maybe UTCTime) "callDate" "Дата звонка"
  , vwcreatedate
    :: F (Maybe UTCTime) "vwcreatedate" "Дата звонка"
  , callTaker
    :: F (IdentI Usermeta)      "callTaker" "Сотрудник РАМК"
  , customerComment
    :: F (Maybe Text) "customerComment" "Неисправность со слов клиента"
  , comment
    :: F (Maybe (IdentI Wazzup)) "comment" "Что случилось"
  , diagnosis1
    :: F (Maybe (IdentI System)) "diagnosis1" "Система"
  , diagnosis2
    :: F (Maybe (IdentI Part)) "diagnosis2" "Узел/деталь"
  , diagnosis3
    :: F (Maybe (IdentI Cause)) "diagnosis3" "Описание причины неисправности"
  , diagnosis4
    :: F (Maybe (IdentI Suggestion)) "diagnosis4" "Рекомендация"
  , contact_name
    :: F (Maybe Text) "contact_name" "Звонящий"
  , contact_phone1
    :: F (Maybe Phone) "contact_phone1" "Телефоны"
  , contact_phone2
    :: F (Maybe Phone) "contact_phone2" ""
  , contact_phone3
    :: F (Maybe Phone) "contact_phone3" ""
  , contact_phone4
    :: F (Maybe Phone) "contact_phone4" ""
  , contact_email
    :: F (Maybe Text) "contact_email" "Email звонящего"
  , contact_contactOwner
    :: F (Maybe Checkbox)     "contact_contactOwner" "Звонящий владелец?"
  , contact_ownerName
    :: F (Maybe Text) "contact_ownerName" "Владелец"
  , contact_ownerPhone1
    :: F (Maybe Phone) "contact_ownerPhone1" "Контактные телефоны владельца"
  , contact_ownerPhone2
    :: F (Maybe Phone) "contact_ownerPhone2" ""
  , contact_ownerPhone3
    :: F (Maybe Phone) "contact_ownerPhone3" ""
  , contact_ownerPhone4
    :: F (Maybe Phone) "contact_ownerPhone4" ""
  , contact_ownerEmail
    :: F (Maybe Text) "contact_ownerEmail" "Email владельца"
  , program
    :: F (IdentI Program) "program" "Программа"
  , subprogram
    :: F (Maybe (IdentI SubProgram))  "subprogram" "Подпрограмма"

  , contractIdentifier
    :: F (Maybe Text) "contractIdentifier" "Идентификатор контракта"
  , contract
    :: F (Maybe (IdentI Contract)) "contract" "Контракт"
  , car_vin
    :: F (Maybe Text) "car_vin" "Автомобиль (VIN)"
  , car_make
    :: F (Maybe (IdentI CarMake)) "car_make" "Марка"
  , car_model
    :: F (Maybe (IdentI CarModel)) "car_model" "Модель"
  , car_seller
    :: F (Maybe (IdentI Partner)) "car_seller" "Дилер, продавший автомобиль"
  , car_plateNum
    :: F (Maybe Text) "car_plateNum" "Госномер"
  , car_makeYear
    :: F (Maybe Int) "car_makeYear" "Год производства автомобиля"
  , car_color
    :: F (Maybe Text) "car_color" "Цвет"
  , car_buyDate
    :: F (Maybe Day) "car_buyDate" "Дата покупки"
  , car_dealerTO
    :: F (Maybe (IdentI Partner)) "car_dealerTO" "Дилер, у которого проходило последнее ТО"
  , car_mileage
    :: F (Maybe Int) "car_mileage" "Текущий пробег"
  , car_transmission
    :: F (Maybe (IdentI Transmission)) "car_transmission" "Коробка передач"
  , car_engine
    :: F (Maybe (IdentI Engine)) "car_engine" "Тип двигателя"
  , car_liters
    :: F (Maybe Text) "car_liters" "Объём двигателя"
  , car_class
    :: F (Maybe (IdentI CarClass)) "car_class" "Класс автомобиля"

  , vinChecked
    :: F (Maybe (IdentI ContractCheckStatus)) "vinChecked" "Участие в программе"
  , city
    :: F (Maybe (IdentI City)) "city" "Город"
  , caseAddress_address
    :: F PickerField "caseAddress_address" "Адрес места поломки"
  , caseAddress_comment
    :: F (Maybe Text) "caseAddress_comment" "Примечания"
  , caseAddress_notRussia
    :: F (Maybe Checkbox) "caseAddress_notRussia" "Не по РФ"
  , caseAddress_coords
    :: F PickerField "caseAddress_coords" "Координаты"
  , caseAddress_map
    :: F (Maybe MapField) "caseAddress_map" ""
  , temperature
    :: F (Maybe Text) "temperature" "Температура"
  , repair
    :: F (Maybe Day) "repair" "Дата починки"
  , accord
    :: F (Maybe Text) "accord" "Номер согласования"
  , dealerCause
    :: F (Maybe Text) "dealerCause" "Неисправность со слов дилера/партнёра"
  , caseStatus
    :: F (IdentI CaseStatus) "caseStatus" "Статус кейса"
  , psaExportNeeded
    :: F (Maybe Checkbox) "psaExportNeeded" "Требуется выгрузка в PSA"
  , psaExported
    :: F (Maybe Checkbox) "psaExported" "Выгружен в PSA"
  , claim
    :: F (Maybe Text) "claim" "Претензия / Благодарность"
  , services
    :: F (Maybe Reference) "services" "Услуги"
  , files
    :: F (Maybe Reference) "files" "Прикрепленные файлы"
  , source
    :: F (IdentI CaseSource) "source" "Источник кейса"
  } deriving Typeable
