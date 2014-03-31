{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Carma.Model.Contract
    ( Contract(..)
    , identifiers
    , identifierNames
    , WDay
    , contractSearchParams
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
import Carma.Model.Colors as Color (label)
import Carma.Model.LegalForm    (LegalForm)
import Carma.Model.Partner      (Partner)
import Carma.Model.SubProgram   (SubProgram)
import Carma.Model.Transmission (Transmission)
import Carma.Model.Usermeta     (Usermeta, value)
import Carma.Model.Engine       (Engine)
import Carma.Model.Search


-- | Transparent 'Day' wrapper so that @typeOf WDay@ points to this
-- module (original name is hidden: @Data.Time.Calendar.Days.Day@).
newtype WDay = WDay Day deriving (FromField, ToField,
                                  FromJSON, ToJSON,
                                  DefaultFieldView,
                                  Typeable)

instance PgTypeable WDay where
  pgTypeOf _ = pgTypeOf (undefined :: Day)


data Contract = Contract
  { ident            :: PK Int Contract ""
  , ctime            :: F UTCTime
                        "ctime"
                        "Время создания контракта"
  , isActive         :: F Bool
                        "isActive"
                        "Активен"
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
  , color            :: F (Maybe Text)
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
  -- This references Usermeta object, not Snap.Auth user!
  , committer        :: F (IdentI Usermeta)
                        "committer"
                        "Пользователь, внёсший данные"
  , dixi             :: F Bool
                        "dixi"
                        "Сохранить"
  } deriving Typeable


instance Model Contract where
  type TableName Contract = "Contract"
  modelInfo = mkModelInfo Contract ident
  modelView = \case
    "search" ->
        Just $ subDict "prefixedSubPrograms" $
        searchView (contractSearchParams)
    "portalSearch" ->
        Just $ subDict "portalSubPrograms" $
        searchView (contractSearchParams)
    ""       ->
        Just $ subDict "prefixedSubPrograms" $
        modifyView defaultView
        [ setMeta "dictionaryParent" "make" model
        , setMeta "dictionaryLabel"
          (String $ Data.Model.fieldName Carma.Model.Usermeta.value)
          committer
        , setMeta "regexp" "email" email
        , setMeta "regexp" "phone" phone
        , setMeta "regexp" "plateNum" plateNum
        , setMeta "regexp" "vin" vin
        , widget "checkbutton" dixi
        , color `completeWith` Color.label
        ]
    _ -> Nothing
    where
      -- Make subprogram field more usable on client
      subDict n v =
          modifyView v
          [ dict subprogram $
            (dictOpt n){ dictType = Just "ComputedDict", dictBounded = True}
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


-- | List of searchable Contract fields.
contractSearchParams :: [(Text, [Predicate Contract])]
contractSearchParams =
    (Data.Model.fieldName subprogram, one subprogram):
    (map (\p@(FA f) -> (fieldNameE p, fuzzy $ one $ f)) identifiers)
