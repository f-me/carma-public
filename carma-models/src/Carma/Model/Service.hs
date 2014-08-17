module Carma.Model.Service where

import qualified Data.Aeson as Aeson

import Data.Text
import Data.Time.Clock
import Data.Typeable

import Data.Model
import Data.Model.Types ((:@))
import Data.Model.View

import qualified Carma.Model.ClientRefusalReason as CRR
import           Carma.Model.Case.Type           (Case)
import           Carma.Model.FalseCall           (FalseCall)
import           Carma.Model.LegacyTypes
import           Carma.Model.PaymentType         (PaymentType)
import           Carma.Model.Satisfaction        (Satisfaction)
import           Carma.Model.Search as S
import           Carma.Model.ServiceStatus       (ServiceStatus)
import           Carma.Model.ServiceType         (ServiceType)
import           Carma.Model.Types (TInt)

data Service = Service
  -- FIXME: ident can be null in pg
  { ident                        :: PK Int Service ""
  , svcType                      :: F (IdentI ServiceType) "type"
                                 "Услуга"
  , parentId                     :: F (IdentI Case) "parentId"
                                 ""
  , createTime                   :: F (Maybe LegacyDatetime) "createTime"
                                 "Дата создания услуги"
  , payType                      :: F (Maybe (IdentI PaymentType)) "payType"
                                 "Тип оплаты"
  , payment_costTranscript       :: F (Maybe Text) "payment_costTranscript"
                                 "Расшифровка стоимости"
  , payment_partnerCost          :: F (Maybe TInt) "payment_partnerCost"
                                 "Стоимость со слов партнёра (число)"
  , payment_calculatedCost       :: F (Maybe TInt) "payment_calculatedCost"
                                 "Расчётная стоимость"
  , payment_limitedCost          :: F (Maybe TInt) "payment_limitedCost"
                                 "Предельная стоимость"
  , payment_overcosted           :: F (Maybe Checkbox) "payment_overcosted"
                                 "Стоимость превышена?"
  , payment_paidByRUAMC          :: F (Maybe Text) "payment_paidByRUAMC"
                                 "Оплата РАМК"
  , payment_paidByClient         :: F (Maybe Text) "payment_paidByClient"
                                 "Оплата Клиент"
  , times_expectedServiceStart   :: F (Maybe UTCTime) "times_expectedServiceStart"
                                 "Ожидаемое время начала оказания услуги"
  , times_expectedDispatch       :: F (Maybe UTCTime) "times_expectedDispatch"
                                 "Время выезда партнёра"
  , times_factServiceStart       :: F (Maybe UTCTime) "times_factServiceStart"
                                 "Фактическое время начала оказания услуги"
  , times_expectedServiceEnd     :: F (Maybe UTCTime) "times_expectedServiceEnd"
                                 "Ожидаемое время окончания оказания услуги"
  , times_factServiceEnd         :: F (Maybe UTCTime) "times_factServiceEnd"
                                 "Фактическое время окончания оказания услуги"
  , times_expectedServiceClosure :: F (Maybe UTCTime) "times_expectedServiceClosure"
                                 "Ожидаемое время закрытия услуги"
  , times_factServiceClosure     :: F (Maybe UTCTime) "times_factServiceClosure"
                                 "Фактическое время закрытия услуги"
  , falseCall                    :: F (IdentI FalseCall) "falseCall"
                                 "Ложный вызов"
  , clientCancelReason           :: F (Maybe Text) "clientCancelReason"
                                 "Причина отказа клиента"
  -- , falseCallPercent             :: F (Maybe Text) "falseCallPercent" ""
  , bill_billNumber              :: F (Maybe Text) "bill_billNumber"
                                 "Номер счёта"
  , bill_billingCost             :: F (Maybe TInt) "bill_billingCost"
                                 "Сумма по счёту"
  , bill_billingDate             :: F (Maybe LegacyDate) {-FIXME: day-} "bill_billingDate"
                                 "Дата выставления счёта"

  , contractor_partner           :: F (Maybe Text) "contractor_partner"
                                 "Партнёр"
  , contractor_partnerId         :: F (Maybe Text) "contractor_partnerId"
                                 "Партнёр"
  , contractor_address           :: F (Maybe Text) "contractor_address"
                                 "Адрес"
  , contractor_coords            :: F (Maybe Text) "contractor_coords"
                                 "Координаты"
  -- , cost_counted                 :: F (Maybe Int) "cost_counted"
  --                                "Расчётная стоимость"
  -- , cost_serviceTarifOptions     :: F (Maybe Reference) "cost_serviceTarifOptions"
  --                                "Тарифные опции"
  -- , marginalCost                 :: F (Maybe Text) "marginalCost"
  --                                "Предельная стоимость"

  , paid                         :: F (Maybe Checkbox) "paid"
                                 "Оплата"
  , scan                         :: F (Maybe Checkbox) "scan"
                                 "Скан загружен"
  , original                     :: F (Maybe Checkbox) "original"
                                 "Оригинал получен"
  , urgentService                :: F (Maybe (IdentT UrgentServiceReason)) "urgentService"
                                 "Приоритетная услуга"
  , status                       :: F (IdentI ServiceStatus) "status"
                                 "Статус услуги"
  , clientSatisfied              :: F (Maybe (IdentI Satisfaction)) "clientSatisfied"
                                 "Клиент доволен"
  , warrantyCase                 :: F (Maybe Checkbox) "warrantyCase"
                                 "Гарантийный случай"
  , files                        :: F (Maybe Reference) "files"
                                 "Прикрепленные файлы"
  -- , service_tarifOptions         :: F (Maybe Reference) "service_tarifOptions"
  --                                ""
  , assignedTo                   :: F (Maybe Text) "assignedTo" ""
  }
  deriving Typeable


instance Model Service where
  type TableName Service = "servicetbl"
  modelInfo = mkModelInfo Service ident `withLegacyName` "service"
  modelView = \case
    "search" -> Just $ modifyView (searchView serviceSearchParams) svcMod
    "full"   -> Just $ modifyView defaultView svcMod
    "new"    -> Just $ modifyView defaultView
      $ usualSvcMod
      ++ [mainOnly times_expectedServiceStart
         ,mainOnly times_expectedServiceEnd
         ,mainOnly times_expectedDispatch
         ]
    "" -> Just $ modifyView defaultView usualSvcMod
    _  -> Nothing


svcMod :: [(Text, FieldView -> FieldView) :@ Service]
svcMod =
    [dict contractor_partnerId $ (dictOpt "allPartners")
          { dictType    = Just "ComputedDict"
          , dictBounded = True
          }
    ,setType "dictionary" contractor_partnerId
    ,setMeta "widget" "partner" contractor_partner
    ,invisible contractor_coords
    ,invisible parentId
    ,invisible assignedTo
    , readonly status
    , setType "text" payment_partnerCost
    , setType "text" payment_calculatedCost
    , setType "text" payment_limitedCost
    , setType "text" bill_billingCost
    , setMeta "dictionaryStringify" (Aeson.Bool True) payType
    , setMeta "dictionaryStringify" (Aeson.Bool True) status
    , clientCancelReason `completeWith` CRR.label
    ]

-- | Mods that shouldn't be appied to search view
usualSvcMod :: [(Text, FieldView -> FieldView) :@ Service]
usualSvcMod = svcMod ++
            [ invisible contractor_partnerId
            , invisible svcType
            ]

serviceSearchParams :: [(Text, [Predicate Service])]
serviceSearchParams
  = [("Service_createtime",   interval createTime)
    ,("contractor_partnerId", one contractor_partnerId)
    ,("svcType",              listOf svcType)
    ]
