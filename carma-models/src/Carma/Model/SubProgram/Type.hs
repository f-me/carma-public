{-| Subprogram and subordinate models. -}

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

import Carma.Model.Types (TInt, IdentList)
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
  , value       :: F Text
                   "value"
                   "Внутренняя метка"
  , mailAddr    :: F (Maybe Text)
                   "mailAddr"
                   "Mail для отправки писем"
  , mailPass    :: F (Maybe Text)
                   "mailPass"
                   "Пароль для отправки писем"
  , contacts    :: F (Maybe (IdentList SubProgramContact))
                   "contacts"
                   "Контактные лица"
  , services    :: F (Maybe (IdentList SubProgramService))
                   "services"
                   "Услуги, предоставляемые по программе"
  , checkPeriod :: F (Maybe TInt)
                   "checkPeriod"
                   "Межсервисный интервал по умолчанию"
  , validFor    :: F (Maybe TInt)
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
  , contractPrs :: F (Maybe (IdentList SubProgramContractPermission))
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
    -- creates and empty instance, then rendering a form where the
    -- type may be selected.
    , sType       :: F (Maybe (IdentI ServiceType)) "type" "Услуга"
    , maxCost     :: F (Maybe Text)
                     "maxCost"
                     "Лимит стоимости"
    , maxDistance :: F (Maybe TInt)
                     "maxDistance"
                     "Лимит расстояния"
    , maxPeriod   :: F (Maybe TInt)
                     "maxPeriod"
                     "Лимит продолжительности (в днях)"
    , maxCount    :: F (Maybe TInt)
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
