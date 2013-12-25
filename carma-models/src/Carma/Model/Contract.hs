module Carma.Model.Contract where

import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types (TInt)

import Carma.Model.CarClass     (CarClass)
import Carma.Model.CarMake      (CarMake)
import Carma.Model.CarModel     (CarModel)
import Carma.Model.CheckType    (CheckType)
import Carma.Model.Colors       (Colors)
import Carma.Model.LegalForm    (LegalForm)
import Carma.Model.Partner      (Partner)
import Carma.Model.SubProgram   (SubProgram)
import Carma.Model.Transmission (Transmission)
import Carma.Model.Usermeta     (Usermeta)
import Carma.Model.Engine       (Engine)


data Contract = Contract
  { ident            :: PK Int Contract ""
  , name             :: F (Maybe Text)
                        "name"
                        "ФИО клиента"
  , email            :: F (Maybe Text)
                        "email"
                        "E-mail клиента"
  , vin              :: F (Maybe Text)
                        "vin"
                        "VIN"
  , cardNumber       :: F (Maybe Text)
                        "cardNumber"
                        "Номер карты"
  , codeWord         :: F (Maybe Text)
                        "codeWord"
                        "Кодовое слово"
  , phone            :: F (Maybe Text)
                        "phone"
                        "Номер телефона"
  , plateNum         :: F (Maybe Text)
                        "plateNum"
                        "Госномер"
  , validSince       :: F (Maybe Day)
                        "validSince"
                        "Дата регистрации в программе"
  , validUntil       :: F (Maybe Day)
                        "validUntil"
                        "Программа действует до (Дата)"
  , startMileage     :: F (Maybe TInt)
                        "startMileage"
                        "Пробег при регистрации в программе"
  -- This is not redundant as car model may be unknown/unrecognized.
  , make             :: F (Maybe (IdentI CarMake))
                        "make"
                        "Марка"
  , model            :: F (Maybe (IdentI CarModel))
                        "model"
                        "Модель"
  -- TODO New Year (pun intended) field type
  , makeYear         :: F (Maybe TInt)
                        "makeYear"
                        "Год производства автомобиля"
  , carClass         :: F (Maybe (IdentI CarClass))
                        "carClass"
                        "Класс автомобиля"
  -- TODO Unbounded dictionary #1305
  , color            :: F (Maybe (IdentT Colors))
                        "color"
                        "Цвет"
  , transmission     :: F (Maybe (IdentI Transmission))
                        "transmission"
                        "Коробка передач"
  , engineVolume     :: F (Maybe Text)
                        "engineVolume"
                        "Объём двигателя"
  , engineType       :: F (Maybe (IdentI Engine))
                        "engineType"
                        "Тип двигателя"
  , buyDate          :: F (Maybe Day)
                        "buyDate"
                        "Дата покупки"
  , seller           :: F (Maybe (IdentI Partner))
                        "seller"
                        "Дилер, продавший автомобиль"
  , lastCheckDealer  :: F (Maybe (IdentI Partner))
                        "lastCheckDealer"
                        "Дилер, у которого проходило последнее ТО"
  , lastCheckMileage :: F (Maybe TInt)
                        "lastCheckMileage"
                        "Пробег на последнем ТО"
  , lastCheckDate    :: F (Maybe Day)
                        "lastCheckDate"
                        "Дата последнего ТО"
  , checkPeriod      :: F (Maybe TInt)
                        "checkPeriod"
                        "Межсервисный интервал"
  , checkType        :: F (Maybe (IdentI CheckType))
                        "checkType"
                        "Вид ТО"
  , orderNumber      :: F (Maybe Text)
                        "orderNumber"
                        "Номер заказ-наряда"
  , managerName      :: F (Maybe Text)
                        "managerName"
                        "ФИО менеджера"
  , comment          :: F (Maybe Text)
                        "comment"
                        "Комментарий"
  , subprogram       :: F (Maybe (IdentI SubProgram))
                        "subprogram"
                        "Подпрограмма"
  , legalForm        :: F (Maybe (IdentI LegalForm))
                        "legalForm"
                        "Физическое/юридическое лицо"
  , committer        :: F (IdentI Usermeta)
                        "committer"
                        "Пользователь, внёсший данные"
  , dixi             :: F Bool
                        "dixi"
                        ""
  , isActive         :: F Bool
                        "isActive"
                        "Активен"
  , ctime            :: F UTCTime
                        "ctime"
                        "Время создания контракта"
  } deriving Typeable


instance Model Contract where
  type TableName Contract = "Contract"
  modelInfo = mkModelInfo Contract ident
  modelView _ = modifyView defaultView
                [ setMeta "dictionaryParent" "make" model
                , setMeta "regexp" "email" email
                , setMeta "regexp" "phone" phone
                , setMeta "regexp" "plateNum" plateNum
                , setMeta "regexp" "vin" vin
                ]
