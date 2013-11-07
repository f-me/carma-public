
module Carma.Model.Service where

import Control.Applicative
import Data.Text
import Data.Typeable
import Data.Time.Clock (UTCTime)
import Data.Aeson
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Data.Model
import Data.Model.View
import Data.Model.Types
import Data.Model.CoffeeType


data Service = Service
  { ident                        :: PK Int Service
  , svcType                      :: F Text "type"
                                 "service type"
  , parentId                     :: F Text "parentId"
                                 ""
  , createTime                   :: F UTCTime "createTime"
                                 "Дата создания услуги"
  , payType                      :: F Text {-FIXME-} "payType"
                                 "Тип оплаты"
  , payment_costTranscript       :: F Text "payment_costTranscript"
                                 "Расшифровка стоимости"
  , payment_partnerCost          :: F Text "payment_partnerCost"
                                 "Стоимость со слов партнёра (число)"
  , payment_calculatedCost       :: F Text "payment_calculatedCost"
                                 "Расчётная стоимость"
  , payment_limitedCost          :: F Text "payment_limitedCost"
                                 "Предельная стоимость"
  , payment_overcosted           :: F Bool "payment_overcosted"
                                 "Стоимость превышена?"
  , payment_paidByRUAMC          :: F Text "payment_paidByRUAMC"
                                 "Оплата РАМК"
  , payment_paidByClient         :: F Text "payment_paidByClient"
                                 "Оплата Клиент"
  , times_expectedServiceStart   :: F UTCTime "times_expectedServiceStart"
                                 "Ожидаемое время начала оказания услуги"
  , times_expectedDispatch       :: F UTCTime "times_expectedDispatch"
                                 "Время выезда партнёра"
  , times_factServiceStart       :: F UTCTime "times_factServiceStart"
                                 "Фактическое время начала оказания услуги"
  , times_expectedServiceEnd     :: F UTCTime "times_expectedServiceEnd"
                                 "Ожидаемое время окончания оказания услуги"
  , times_factServiceEnd         :: F UTCTime "times_factServiceEnd"
                                 "Фактическое время окончания оказания услуги"
  , times_expectedServiceClosure :: F UTCTime "times_expectedServiceClosure"
                                 "Ожидаемое время закрытия услуги"
  , times_factServiceClosure     :: F UTCTime "times_factServiceClosure"
                                 "Фактическое время закрытия услуги"
  , falseCall                    :: F Text {-FIXME-} "falseCall"
                                 "Ложный вызов"
  , clientCancelReason           :: F Text {-FIXME-} "clientCancelReason"
                                 "Причина отказа клиента"
  , falseCallPercent             :: F Text "falseCallPercent"
                                 ""
  , bill_billNumber              :: F Text "bill_billNumber"
                                 "Номер счёта"
  , bill_billingCost             :: F Text "bill_billingCost"
                                 "Сумма по счёту"
  , bill_billingDate             :: F UTCTime {-FIXME: day-} "bill_billingDate"
                                 "Дата выставления счёта"

  , contractor_partner           :: F Text "contractor_partner"
                                 "Партнёр"
  , contractor_partnerId         :: F Text "contractor_partnerId"
                                 ""
  , contractor_address           :: F Text "contractor_address"
                                 "Адрес"
  , contractor_coords            :: F Text "contractor_coords"
                                 "Координаты"
  , cost_counted                 :: F Text "cost_counted"
                                 "Расчётная стоимость"
  , cost_serviceTarifOptions     :: F Reference "cost_serviceTarifOptions"
                                 "Тарифные опции"
  , marginalCost                 :: F Text "marginalCost"
                                 "Предельная стоимость"

  , paid                         :: F Bool "paid"
                                 "Оплата"
  , scan                         :: F Bool "scan"
                                 "Скан загружен"
  , original                     :: F Bool "original"
                                 "Оригинал получен"
  , urgentService                :: F Text {-FIXME-} "urgentService"
                                 "Приоритетная услуга"
  , status                       :: F Text {-FIXME-} "status"
                                 "Статус услуги"
  , clientSatisfied              :: F Text {-FIXME-} "clientSatisfied"
                                 "Клиент доволен"
  , warrantyCase                 :: F Bool "warrantyCase"
                                 "Гарантийный случай"
  , files                        :: F Reference "files"
                                 "Прикрепленные файлы"
  , service_tarifOptions         :: F Reference "service_tarifOptions"
                                 ""
  , assignedTo                   :: F Text "assignedTo"
                                 ""
  }
  deriving Typeable


data Reference = Reference Text deriving Typeable
instance CoffeeType Reference where
  coffeeType = Wrap "reference"
instance FromJSON Reference where
  parseJSON fld = Reference <$> parseJSON fld
instance ToJSON Reference where
  toJSON (Reference txt) = toJSON txt
instance ToField Reference where
  toField (Reference txt) = toField txt
instance FromField Reference where
  fromField fld m = Reference <$> fromField fld m


instance Model Service where
  type TableName Service = "servicetbl"
  modelInfo = mkModelInfo Service ident
  modelView _ = defaultView
