{-# LANGUAGE ConstraintKinds #-}

module Carma.Model.Partner
    ( Partner(..)
    , partnerKey
    ) where

import Data.Aeson as A
import Data.Text (Text)
import Data.Typeable
import Data.Vector
import Data.Time.Clock (UTCTime)

import Data.Model
import Data.Model.View
import Data.Model.Types

import Carma.Model.Types (Coords)
import Carma.Model.LegacyTypes (Reference)
import Carma.Model.CarMake (CarMake)
import Carma.Model.City (City)
import Carma.Model.TaxScheme (TaxScheme)


data Partner = Partner
  { ident    :: PK Int Partner "Партнёр"
  , isActive :: F Bool         "isActive" "Партнёр активен"
  , isDealer :: F Bool         "isDealer" "Дилер"
  , isMobile :: F Bool         "isMobile" "Мобильный партнёр"
  , isFree   :: F Bool         "isFree"   "Свободен"
  , name     :: F Text         "name"     "Название"
  , synonyms :: F (Vector Text)"synonyms" "Синонимы"
  , code     :: F (Maybe Text) "code"     "Код"
  , city     :: F (Maybe (IdentI City)) "city" "Город"
  , makes    :: F (Vector (IdentI CarMake)) "makes" "Обслуживаемые марки"
  , phones   :: F A.Value      "phones"   "Телефоны"
  , coords   :: F (Maybe Coords)"coords"  "Координаты фактического адреса"
  , addrs    :: F A.Value       "addrs"   "Адреса"
  , emails   :: F A.Value       "emails"  "E-mail"
  , personInCharge
             :: F (Maybe Text) "personInCharge" "Ответственное лицо"
  , taxScheme
             :: F (Maybe (IdentI TaxScheme)) "taxScheme" "Форма налогообложения"
  , isPayBackConfirmed
             :: F Bool         "isPayBackConfirmed" "Соглашение о вознаграждении"
  , foreignIdent
             :: F (Maybe Text) "foreignIdent" "Внешний код партнёра"
  , mtime    :: F UTCTime      "mtime" ""
  , services :: F A.Value      "services" "Услуги"
  , comment  :: F Text         "comment" "Комментарий"
  }
  deriving Typeable

instance Model Partner where
  type TableName Partner = "partnertbl"
  modelInfo = mkModelInfo Partner ident
  modelView = \case
    "" -> Just $ modifyView defaultView
      [required makes
      ,setMeta "noteLabel"        "Время работы"     phones
      ,setMeta "showNote"         (A.Bool True)      phones
      ,regexp regexpPhone                            phones
      ,setMeta "addLabel"         "Добавить телефон и время работы"
                                                     phones
      ,setMeta "jsonSchema"       "dict-objects"     phones
      ,setMeta "dictionaryName"   "PhoneTypes"       phones
      ,setMeta "widget"           "dict-objects"     phones

      ,setMeta "widget"           "picker"           coords
      ,setMeta "cityField"        "city"             coords
      ,setMeta "infoText"         "coords"           coords
      ,setMeta "required"         (A.Bool True)      coords
      ,setMeta "targetCoords"     "coords"           coords
      ,setMeta "targetAddr"       "factAddr"         coords
      ,setMeta "picker"           "mapPicker"        coords
      ,setMeta "currentBlipType"  "partner"          coords

      ,setMeta "addLabel"         "Добавить адрес"   addrs
      ,setMeta "jsonSchema"       "dict-objects"     addrs
      ,setMeta "dictionaryName"   "AddressTypes"     addrs
      ,setMeta "widget"           "dict-objects"     addrs

      ,regexp regexpEmail                            emails
      ,setMeta "addLabel"         "Добавить e-mail"  emails
      ,setMeta "jsonSchema"       "dict-objects"     emails
      ,setMeta "dictionaryName"   "EmailTypes"       emails
      ,setMeta "widget"           "dict-objects"     emails

      ,setMeta "widget"           "partner_services" services
      ,textarea comment
      ,invisible mtime
      ]
    _  -> Nothing


-- | Set proper @dictionaryLabel@ meta for a field referring to
-- 'Partner'.
partnerKey
  :: FieldI t n d
  => (m -> F t n d) -> (Text, FieldView -> FieldView) :@ m
partnerKey f = setMeta "dictionaryLabel" (A.String $ fieldName name) f
