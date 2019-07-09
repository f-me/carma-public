{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, ExistentialQuantification #-}

module Carma.Model.Contract
     ( Contract(..)
     , identifiers
     , identifierNames
     , WDay(..)
     , contractSearchParams
     , contractCaseSearchParams
     ) where

import GHC.Generics

import Data.Aeson
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Data.Typeable
import Data.Scientific (Scientific)
import Data.Singletons

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField

import Data.Model as Model
import Data.Model as DM
import Data.Model.Types
import Data.Model.View

import Carma.Model.CarClass     (CarClass)
import Carma.Model.CarGeneration (CarGeneration)
import Carma.Model.CarMake      (CarMake)
import Carma.Model.CarModel     (CarModel)
import Carma.Model.CheckType    (CheckType)
import Carma.Model.Colors as Color (label)
import Carma.Model.LegalForm    (LegalForm)
import Carma.Model.Partner      (Partner, partnerKey)
import Carma.Model.Search
import Carma.Model.SubProgram   (SubProgram)
import Carma.Model.Transmission (Transmission)
import Carma.Model.Usermeta     as Usermeta (Usermeta, realName)
import Carma.Model.Engine       (Engine)
import Carma.Model.ContractRegistrationReason (ContractRegistrationReason)


-- | Transparent 'Day' wrapper so that @typeOf WDay@ points to this
-- module (original name is hidden: @Data.Time.Calendar.Days.Day@).
newtype WDay = WDay { unWDay :: Day } deriving (Eq, FromField, ToField,
                                                FromJSON, ToJSON,
                                                Show, Typeable, Ord)

instance DefaultFieldView WDay where
  defaultFieldView (_ :: m -> FF WDay n d a) =
    defaultFieldView (undefined :: m -> FF Day n d a)

instance PgTypeable WDay where
  pgTypeOf _ = pgTypeOf (undefined :: Day)

instance forall m nm desc . (SingI nm, SingI desc, Model m)
         => IntervalPred (m -> F WDay nm desc) m  where
  interval _ = interval (undefined :: (m -> F Day nm desc)) :: [Predicate m]


-- REMEMBER to update "Contract_csv" view and probably VinFormat
-- fields when changing this model!
data Contract = Contract
  { ident            :: PK Int Contract "№ контракта"
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
  , fromArc          :: F Bool
                        "fromArc"
                        "Подгружен из ARC"
  , sourceFile       :: F Text
                        "sourceFile"
                        "Название файла-источника"
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
  , startMileage     :: F (Maybe Int)
                        "startMileage"
                        "Пробег при регистрации в программе"
  -- This is not redundant as car model may be unknown/unrecognized.
  , make             :: F (Maybe (IdentI CarMake))
                        "make"
                        "Марка"
  , model            :: F (Maybe (IdentI CarModel))
                        "model"
                        "Модель"
  , generation       :: F (Maybe (IdentI CarGeneration))
                        "generation"
                        "Поколение"
  -- TODO New Year (pun intended) field type
  , makeYear         :: F (Maybe Int)
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
  , firstSaleDate    :: F (Maybe WDay)
                        "firstSaleDate"
                        "Дата первой продажи"
  , seller           :: F (Maybe (IdentI Partner))
                        "seller"
                        "Дилер, продавший автомобиль"
  , registrationReason
                     :: F (Maybe (IdentI ContractRegistrationReason))
                        "registrationReason"
                        "Основание для регистрации в программе"
  , priceInOrder     :: F (Maybe Scientific)
                        "priceInOrder"
                        "Стоимость в заказ-наряде"
  , lastCheckDealer  :: F (Maybe (IdentI Partner))
                        "lastCheckDealer"
                        "Дилер, у которого проходило последнее ТО"
  , checkPeriod      :: F (Maybe Int)
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
  , extra            :: F (Maybe Value)
                        "extra"
                        "Дополнительные данные"
  , dixi             :: F Bool
                        "dixi"
                        "Сохранить"
  } deriving (Generic, Typeable)

instance Model Contract where
  type TableName Contract = "Contract"
  modelInfo = mkModelInfo Contract ident
  modelView = \case
    "search" ->
        Just $
        useDict subprogram "prefixedSubPrograms" $
        useDict generation "prefixedGenerations" $
        flip modifyView commonMeta $
        searchView contractSearchParams
    "searchCase" ->
        Just $
        useDict subprogram "prefixedSubPrograms" $
        useDict generation "prefixedGenerations" $
        flip modifyView commonMeta $
        searchView contractCaseSearchParams

    "portalSearch" ->
        Just $
        useDict subprogram "portalSubPrograms" $
        flip modifyView commonMeta $
        searchView contractSearchParams
    "portalForm" ->
        Just $
        useDict subprogram "portalSubPrograms" $
        defaultView `modifyView` commonMeta
    ""       ->
        Just $
        useDict subprogram "prefixedSubPrograms" $
        defaultView `modifyView` commonMeta
    _ -> Nothing
    where
      -- Make a dictionary field more usable on client (include parent
      -- labels)
      useDict f n v =
          modifyView v
          [ dict f $
            (dictOpt n){dictType = Just "ComputedDict", dictBounded = True}
          ]


-- | Meta setters common for standard and search views.
commonMeta :: [(Text, FieldView -> FieldView) :@ Contract]
commonMeta =
    [ setMeta "dictionaryParent" "make" model
    , setMeta "dictionaryParent"
      (String $ Model.fieldName model) generation
    , setMeta "dictionaryLabel"
      (String $ DM.fieldName Usermeta.realName)
      committer
    , readonly committer
    , color `completeWith` Color.label
    , partnerKey seller
    , setMeta "filterBy" "isDealer" seller
    , setType "Integer" ident
    , widget "text" ident
    , partnerKey lastCheckDealer
    , regexp regexpEmail email
    , regexp regexpPhone phone
    , regexp regexpPlateNum plateNum
    , regexp regexpVIN vin
    , regexp regexpMileage startMileage
    , widget "checkbutton" dixi
    , widget "contract_isActive" isActive
    ]


-- | Contract identifiers used to import contracts in bulk and to
-- search contracts on case screen.
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


-- | List of searchable Contract fields available on case search.
contractCaseSearchParams :: [(Text, [Predicate Contract])]
contractCaseSearchParams = [("cardNumber", fuzzy $ one cardNumber)]

-- | List of searchable Contract fields.
--
-- Must include all fields possibly available through portal screen.
contractSearchParams :: [(Text, [Predicate Contract])]
contractSearchParams =
    (map (\p@(FA f) -> (fieldNameE p, one f))
     [ FA subprogram
     , FA isActive
     , FA ident
     , FA make
     , FA model
     , FA generation
     , FA carClass
     , FA transmission
     , FA engineType
     , FA seller
     , FA lastCheckDealer
     , FA checkType
     , FA legalForm
     , FA committer
     , FA startMileage
     , FA checkPeriod
     , FA makeYear
     , FA registrationReason
     , FA priceInOrder
     ]) ++
    (map (\p@(FA f) -> (fieldNameE p, fuzzy $ one f)) $
     identifiers ++
     [ FA color
     , FA engineVolume
     , FA orderNumber
     , FA managerName
     , FA comment
     ]) ++
    [ (DM.fieldName ctime, interval ctime)
    , (DM.fieldName validSince, interval validSince)
    , (DM.fieldName validUntil, interval validUntil)
    , (DM.fieldName buyDate, interval buyDate)
    , (DM.fieldName firstSaleDate, interval firstSaleDate)
    ]
