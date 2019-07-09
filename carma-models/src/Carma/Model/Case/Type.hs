{-# LANGUAGE DeriveGeneric #-}

-- WARNING! This module supposed to be imported only from `Carma.Model.Case`.
--          If you need `Case` type import it from `Carma.Model.Case` instead.

module Carma.Model.Case.Type where

import GHC.Generics (Generic)

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
import Carma.Model.CarGeneration(CarGeneration)
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

    -- TODO Explain what this field for and in which cases it is not null
  , vwcreatedate
    :: F (Maybe UTCTime) "vwcreatedate" "Дата звонка"

  , callTaker
    :: F (IdentI Usermeta) "callTaker" "Сотрудник РАМК"
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
    :: F (Maybe Checkbox) "contact_contactOwner" "Звонящий владелец?"
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
    :: F (Maybe (IdentI SubProgram)) "subprogram" "Подпрограмма"

    -- This field usually have the same value as "car_vin".
    --
    -- Historically there were only big "Program" participants,
    -- and all of their contracts were umambiguously attached to a VIN
    -- (and nowadays it is usual case when "contractIdentifier" is equal to
    -- "car_vin" in context of case-insensetivity, "contractIdentifier" is
    -- filled by drop-down by selection an option and "car_vin" could be
    -- changed by bare hands where you could expect lower and upper case).
    --
    -- Later some "micro-programs" have been appeared, where some association
    -- with a customer playing role as "contractIdentifier", for example a
    -- card could be sold to a customer and then a customer could be identified
    -- by the number of a sold card, and that number will be the value of
    -- "contractIdentifier", or it could be customer's phone number, etc.
    --
    -- But "car_vin" in its turn is definite parameter (VIN) of a car,
    -- so now it's separated field. Anyway, it will be automatically filled by
    -- selecting a "Contract" for a "Case" from drop-down menu of
    -- "contractIdentifier" (from "vin" field of a "Contract"), see
    -- "contractToCase" from "Triggers" module for details.
    --
    -- To summorize we could say "contractIdentifier" is a variate value which
    -- is usually VIN but sometimes it's customer's phone number or a card
    -- number, etc.
  , contractIdentifier
    :: F (Maybe Text) "contractIdentifier" "Идентификатор контракта"
  , contract
    :: F (Maybe (IdentI Contract)) "contract" "Контракт"

    -- See about VIN:
    -- https://en.wikipedia.org/wiki/Vehicle_identification_number
  , car_vin
    :: F (Maybe Text) "car_vin" "Автомобиль (VIN)"

  , car_make
    :: F (Maybe (IdentI CarMake)) "car_make" "Марка"
  , car_model
    :: F (Maybe (IdentI CarModel)) "car_model" "Модель"
  , car_generation
    :: F (Maybe (IdentI CarGeneration)) "car_generation" "Поколение"
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
  , car_firstSaleDate
    :: F (Maybe Day) "car_firstSaleDate" "Дата первой продажи"
  , car_dealerTO
    :: F (Maybe (IdentI Partner))
         "car_dealerTO"
         "Дилер, у которого проходило последнее ТО"
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
  , city -- Also known as "Город ближайший к месту поломки"
    :: F (Maybe (IdentI City)) "city" "Город"
  , caseAddress_city -- Also known as "Город места поломки"
    :: F (Maybe (IdentI City)) "caseAddress_city" "Город"
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
    :: EF Reference "services" "Услуги"
  , files
    :: F (Maybe Reference) "files" "Прикрепленные файлы"
  , source
    :: F (IdentI CaseSource) "source" "Источник кейса"
  , acStart
    :: F (Maybe UTCTime) "acStart" "Время начала постзызывной обработки"
  , isCreatedByEraGlonass
    :: F Bool "isCreatedByEraGlonass" "Заявка от ЭРА-ГЛОНАСС"
  } deriving (Typeable, Generic)
