{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.Role where

import Prelude hiding (all, head)
import Data.Text (Text)
import Data.Typeable
import Data.Model
import Data.Model.View
import Data.Model.TH
import Carma.Model.Types()
import Carma.Model.PgTypes()

data Role = Role
  {ident  :: PK Int Role ""
  ,label  :: F Text "label"  "Название роли"
  ,value  :: F Text "value"  "Внутреннее название роли"
  -- ^ Internal label is only used to set screen permissions.
  ,isBack :: F Bool "isBack" "Роль бэкофиса"
  ,hidden :: F Bool "hidden" "Скрытая"
  } deriving Typeable

mkIdents [t|Role|]
  [ ("core",            1) -- Экран кейса и базовые поля
  , ("call",            2) -- Оператор Front Office
  , ("parguy",          3) -- Администрирование партнёров
  , ("userAdmin",       4) -- Администрирование пользователей
  , ("userViewer",      5) -- Просмотр справочника пользователей
  , ("lovAdmin",        6) -- Администрирование справочников
  , ("lovViewer",       7) -- Просмотр справочников
  , ("reportManager",   8) -- Аналитик по отчётам
  , ("billManager",     9) -- Управляющий счетами
  , ("billChecker",    10) -- Менеджер по счетам
  , ("vinAdmin",       11) -- Загрузка VIN
  , ("supervisor",     12) -- Супервизор
  , ("head",           13) -- Глава РКЦ
  , ("back",           14) -- Работа с бэкофисом
  , ("psaanalyst",     15) -- Аналитик PSA
  , ("searchCase",     16) -- Поиск услуг
  , ("searchCall",     17) -- Поиск звонков
  , ("searchContract", 18) -- Поиск контрактов
  , ("partner",        19) -- Пользователь экрана контрактов
  , ("contract_admin", 20) -- Администратор контрактов
  , ("bo_qa",          22) -- БО: Менеджер по качеству
  , ("bo_order",       23) -- БО: Заказ услуги
  , ("bo_control",     24) -- БО: Контроль услуги
  , ("bo_account",     25) -- БО: Бухгалтер
  , ("bo_director",    26) -- БО: Директор
  , ("bo_analyst",     27) -- БО: Аналитик
  , ("bo_bill",        28) -- БО: Операции со счетами
  , ("bo_parguy",      29) -- БО: Менеджер по партнёрам
  , ("bo_close",       30) -- БО: Закрытие кейсов
  , ("bo_dealer",      31) -- БО: Аналитик по работе с дилерами
  , ("vwfake",         32) -- Секретная роль vwfake
  , ("dpViewer",       34) -- Пользователь экрана ДиП
  , ("programManager", 35) -- Менеджер по программе
  , ("sms",            40) -- Отправка SMS
  , ("bo_secondary",   41) -- БО: Заказ вторичных услуг
  , ("hacker",         42) -- Разработчик
  , ("bo_info",        43) -- БО: Заказ услуги (ТДИ)
  , ("cti",            50) -- Доступ к CTI-панели
  ]

instance Model Role where
  type TableName Role = "Role"
  idents = Carma.Model.Role.idents
  modelInfo = mkModelInfo Role ident
  modelView = \case
    "" -> Just $ modifyView defaultView
                [ readonly value
                , infoText "roleValue" value
                , infoText "roleBack" isBack
                ]
    _  -> Nothing
