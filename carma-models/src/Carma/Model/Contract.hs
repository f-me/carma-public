{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Carma.Model.Contract
    ( Contract(..)
    , identifiers
    , identifierNames
    , WDay
    )

where

import Data.Aeson
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Data.Typeable

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField

import Data.Model
import Data.Model.Types
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


-- | Transparent 'Day' wrapper so that @typeOf WDay@ points to this
-- module (original name is hidden: @Data.Time.Calendar.Days.Day@).
newtype WDay = WDay Day deriving (FromField, ToField,
                                  FromJSON, ToJSON,
                                  DefaultFieldView,
                                  Typeable)


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
  , validSince       :: F (Maybe WDay)
                        "validSince"
                        "Дата регистрации в программе"
  , validUntil       :: F (Maybe WDay)
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
  , buyDate          :: F (Maybe WDay)
                        "buyDate"
                        "Дата покупки"
  , seller           :: F (Maybe (IdentI Partner))
                        "seller"
                        "Дилер, продавший автомобиль"
  , lastCheckDealer  :: F (Maybe (IdentI Partner))
                        "lastCheckDealer"
                        "Дилер, у которого проходило последнее ТО"
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


-- | Contract identifiers used to import/search contracts.
identifiers :: [FA Contract]
identifiers = [ FA vin
              , FA cardNumber
              , FA plateNum
              , FA name
              , FA phone
              , FA codeWord
              , FA email
              ]


-- | List of 'identifiers' field names used to search contracts.
identifierNames :: [Text]
identifierNames = map fieldNameE identifiers
