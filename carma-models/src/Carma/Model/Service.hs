
module Carma.Model.Service where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.Types ((:@))
import Data.Model.View
import Carma.Model.ServiceNames (ServiceNames)
import Carma.Model.Types (TInt)
import Carma.Model.LegacyTypes
import Carma.Model.Search as S

data Service = Service
  -- FIXME: ident can be null in pg
  { ident                        :: PK Int Service ""
  , svcType                      :: F (Maybe (IdentT ServiceNames)) "type"
                                 "Услуга"
  , parentId                     :: F Text "parentId"
                                 ""
  , createTime                   :: F (Maybe LegacyDatetime) "createTime"
                                 "Дата создания услуги"
  , payType                      :: F (Maybe (IdentT PaymentTypes)) "payType"
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
  , times_expectedServiceStart   :: F (Maybe LegacyDatetime) "times_expectedServiceStart"
                                 "Ожидаемое время начала оказания услуги"
  , times_expectedDispatch       :: F (Maybe LegacyDatetime) "times_expectedDispatch"
                                 "Время выезда партнёра"
  , times_factServiceStart       :: F (Maybe LegacyDatetime) "times_factServiceStart"
                                 "Фактическое время начала оказания услуги"
  , times_expectedServiceEnd     :: F (Maybe LegacyDatetime) "times_expectedServiceEnd"
                                 "Ожидаемое время окончания оказания услуги"
  , times_factServiceEnd         :: F (Maybe LegacyDatetime) "times_factServiceEnd"
                                 "Фактическое время окончания оказания услуги"
  , times_expectedServiceClosure :: F (Maybe LegacyDatetime) "times_expectedServiceClosure"
                                 "Ожидаемое время закрытия услуги"
  , times_factServiceClosure     :: F (Maybe LegacyDatetime) "times_factServiceClosure"
                                 "Фактическое время закрытия услуги"
  , falseCall                    :: F (Maybe (IdentT FalseStatuses)) "falseCall"
                                 "Ложный вызов"
  , clientCancelReason           :: F (Maybe (IdentT ClientCancelReason)) "clientCancelReason"
                                 "Причина отказа клиента"
  -- , falseCallPercent             :: F (Maybe Text) "falseCallPercent" ""
  , bill_billNumber              :: F (Maybe Text) "bill_billNumber"
                                 "Номер счёта"
  , bill_billingCost             :: F (Maybe Int) "bill_billingCost"
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
  , status                       :: F (Maybe (IdentT ServiceStatuses)) "status"
                                 "Статус услуги"
  , clientSatisfied              :: F (Maybe (IdentT Satisfaction)) "clientSatisfied"
                                 "Клиент доволен"
  , warrantyCase                 :: F (Maybe Checkbox) "warrantyCase"
                                 "Гарантийный случай"
  , files                        :: F (Maybe Reference) "files"
                                 "Прикрепленные файлы"
  -- , service_tarifOptions         :: F (Maybe Reference) "service_tarifOptions"
  --                                ""
  -- , assignedTo                   :: F (Maybe Text) "assignedTo"
  --                                ""
  }
  deriving Typeable


instance Model Service where
  type TableName Service = "servicetbl"
  modelInfo = mkModelInfo Service ident
  modelView = \case
    "search" -> Just $ modifyView (searchView serviceSearchParams) svcMod
    "full"   -> Just $ modifyView defaultView svcMod
    "new"    -> Just $ modifyView defaultView
      $ svcMod
      ++ [mainOnly times_expectedServiceStart
         ,mainOnly times_expectedServiceEnd
         ,mainOnly times_expectedDispatch
         ]
    "" -> Just $ modifyView defaultView svcMod
    _  -> Nothing


svcMod :: [(Text, FieldView -> FieldView) :@ Service]
svcMod =
    [dict contractor_partnerId $ (dictOpt "allPartners")
          { dictType    = Just "ComputedDict"
          , dictBounded = True
          }
    ,setType "dictionary" contractor_partnerId
    ,setMeta "widget" "partner" contractor_partner
    ,invisible contractor_partnerId
    ,invisible contractor_coords
    ,invisible parentId
    ,invisible svcType
    , setType "text" payment_partnerCost
    , setType "text" payment_calculatedCost
    , setType "text" payment_limitedCost
    , setType "text" bill_billingCost      
    ]


serviceSearchParams :: [(Text, [Predicate Service])]
serviceSearchParams
  = [("Service_createtime",   interval createTime)
    ,("contractor_partnerId", one contractor_partnerId)
    ,("svcType",              listOf svcType)
    ]
