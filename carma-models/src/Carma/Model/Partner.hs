{-# LANGUAGE ConstraintKinds #-}

{-|

Proxy model for a subset of legacy partner model.

TODO JSON & coords Haskell types.

-}

module Carma.Model.Partner
    ( Partner(..)
    , partnerKey
    )

where

import Data.Aeson as A
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View
import Data.Model.Types

import Carma.Model.Types()
import Carma.Model.City hiding (ident)
import Carma.Model.CarMake hiding (ident)

data Partner = Partner
  { ident    :: PK Int Partner "Партнёр"
  , isActive :: F (Maybe Bool) "isActive" "Партнёр активен"
  , isDealer :: F (Maybe Bool) "isDealer" "Дилер"
  , isMobile :: F (Maybe Bool) "isMobile" "Мобильный партнёр"
  , isFree   :: F (Maybe Bool) "isFree" "Свободен"
  , name     :: F Text          "name" "Название"
  , synonyms :: F (Maybe (Vector Text)) "synonyms" "Синонимы"
  , code     :: F (Maybe Text) "code" "Код"
  , city     :: F (Maybe (IdentT City)) "city" "Город"
  , makes
    :: F (Maybe (Vector (IdentT CarMake))) "makes" "Обслуживаемые марки"
--  , phones
--   :: F (Maybe Text) {- json -} "phones" "Телефоны"
--  , coords
--   :: F (Maybe Text) {- coords -} "coords" "Координаты фактического адреса"
--  , addrs
--    :: F (Maybe Text) {- json -} "addrs" "Адреса"
--  , emails
--    :: F (Maybe Text) {- json -} "emails" "E-mail"
--  , personInCharge
--    :: F (Maybe Text) "personInCharge" "Ответственное лицо"
  -- , taxScheme
  --   :: F (Maybe (Ident TaxSchemes)) "taxScheme" "Форма налогообложения"
--  , isPayBackConfirmed
--    :: F (Maybe Bool) "isPayBackConfirmed" "Соглашение о вознаграждении"
--  , mtime
--    :: F (Maybe UTCTime) "mtime" ""
--  , services
--    :: F (Maybe Text) "services" ""
--  , comment
--    :: F (Maybe Text) "comment" "Комментарий"
  }
  deriving Typeable

instance Model Partner where
  type TableName Partner = "partnertbl"
  modelInfo = mkModelInfo Partner ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing

-- | Set proper @dictionaryLabel@ meta for a field referring to
-- 'Partner'.
partnerKey :: FieldI t n d =>
             (m -> F t n d)
          -> (Text, FieldView -> FieldView) :@ m
partnerKey f = setMeta "dictionaryLabel" (A.String $ fieldName name) f
