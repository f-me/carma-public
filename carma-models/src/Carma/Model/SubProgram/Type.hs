{-|

Subprogram and subordinate models.

Model instances are declared in separate modules to prevent ident
collisions.

-}

module Carma.Model.SubProgram.Type
    ( SubProgram(..)
    , SubProgramContact(..)
    , SubProgramContractPermission(..)
    , SubProgramService(..)
    )

where

import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model

import Carma.Model.Types (IdentList)
import Carma.Model.LegacyTypes (Reference)
import Carma.Model.Program hiding (ident)
import Carma.Model.ServiceType hiding (ident)


data SubProgram = SubProgram
  { ident       :: PK Int SubProgram
                   "Подпрограмма"
  , parent      :: F (IdentI Program)
                   "parent"
                   "Программа"
  , label       :: F Text
                   "label"
                   "Название"
  , active      :: F Bool
                   "active"
                   "Активна"
  , leader      :: F Bool
                   "leader"
                   "Основная"
  , synonyms    :: F (Maybe (Vector Text))
                   "synonyms"
                   "Синонимы"
  , mailAddr    :: F (Maybe Text)
                   "mailAddr"
                   "Mail для отправки писем"
  , mailPass    :: F (Maybe Text)
                   "mailPass"
                   "Пароль для отправки писем"
  , contacts    :: F (IdentList SubProgramContact)
                   "contacts"
                   "Контактные лица"
  , services    :: F (IdentList SubProgramService)
                   "services"
                   "Услуги, предоставляемые по программе"
  , checkPeriod :: F (Maybe Int)
                   "checkPeriod"
                   "Межсервисный интервал по умолчанию"
  , validFor    :: F (Maybe Int)
                   "validFor"
                   "Срок действия программы по умолчанию"
  , smsSender   :: F Text
                   "smsSender"
                   "Отправитель SMS"
  , smsContact  :: F Text
                   "smsContact"
                   "Контактный номер телефона в SMS"
  , smsProgram  :: F Text
                   "smsProgram"
                   "Название программы в SMS"
  , contractPrs :: F (IdentList SubProgramContractPermission)
                   "contractPermissions"
                   "Настройка партнёрского интерфейса"
  , template    :: F (Maybe Reference)
                   "template"
                   "Шаблон договора"
  , logo        :: F (Maybe Reference)
                   "logo"
                   "Логотип"
  , help        :: F (Maybe Text)
                   "help"
                   "Справка"
  , dealerHelp  :: F (Maybe Text)
                   "dealerHelp"
                   "Справка для дилеров"
  } deriving Typeable


data SubProgramService = SubProgramService
    { sIdent      :: PK Int SubProgramService "Услуга по подпрограмме"
    , sParent     :: F (IdentI SubProgram) "parent" "Подпрограмма"
    -- TODO This is wrapped in Maybe only because the client first
    -- creates an empty instance, then rendering a form where the type
    -- may be selected.
    , sType       :: F (Maybe (IdentI ServiceType)) "type" "Услуга"
    , maxCost     :: F (Maybe Text)
                     "maxCost"
                     "Лимит стоимости"
    , maxDistance :: F (Maybe Int)
                     "maxDistance"
                     "Лимит расстояния"
    , maxPeriod   :: F (Maybe Int)
                     "maxPeriod"
                     "Лимит продолжительности (в днях)"
    , maxCount    :: F (Maybe Int)
                     "maxCount"
                     "Лимит количества предоставления услуги"
    } deriving Typeable


data SubProgramContractPermission = SubProgramContractPermission
    { fIdent     :: PK Int SubProgramContractPermission
                    "Ограничение на поле контракта"
    , fParent    :: F (IdentI SubProgram)
                   "parent"
                   "Подпрограмма"
    -- TODO This must be limited to Contract field names only.
    --
    -- Wrapped in Maybe just like the type field in SubProgramService.
    , field     :: F (Maybe Text)
                   "contractField"
                   "Тип поля"
    , showTable :: F Bool
                   "showTable"
                   "Отображается в таблице"
    , showForm  :: F Bool
                   "showForm"
                   "Отображается в форме"
    } deriving Typeable


data SubProgramContact = SubProgramContact
    { cIdent      :: PK Int SubProgramContact "Контактное лицо"
    , cParent     :: F (IdentI SubProgram) "parent" "Подпрограмма"
    , name        :: F (Maybe Text)        "name"   "ФИО"
    , email       :: F (Maybe Text)        "email"  "E-mail"
    , phone       :: F (Maybe Text)        "phone"  "Телефон"
    } deriving Typeable
