module Carma.Model.Service where

import Data.Aeson as Aeson
import Data.Text
import Data.Time
import Data.Typeable

import Data.Model
import Data.Model.Types ((:@))
import Data.Model.View

import qualified Carma.Model.ClientRefusalReason as CRR
import           Carma.Model.Case.Type           (Case)
import           Carma.Model.Complication        (Complication)
import           Carma.Model.FalseCall           (FalseCall)
import           Carma.Model.LegacyTypes
import           Carma.Model.Partner             (Partner)
import           Carma.Model.PaymentType         (PaymentType)
import           Carma.Model.Satisfaction        (Satisfaction)
import           Carma.Model.Search as S
import           Carma.Model.ServiceStatus       (ServiceStatus)
import           Carma.Model.ServiceType         (ServiceType)
import           Carma.Model.UrgentServiceReason (UrgentServiceReason)
import           Carma.Model.Usermeta            (Usermeta)

data Service = Service
  { ident                        :: PK Int Service ""
  , svcType                      :: F (IdentI ServiceType) "type"
                                 "Услуга"
  , parentId                     :: F (IdentI Case) "parentId"
                                 ""
  , createTime                   :: F (Maybe UTCTime) "createTime"
                                 "Дата создания услуги"
  , creator                      :: F (IdentI Usermeta) "creator"
                                 "Сотрудник, создавший услугу"
  , payType                      :: F (Maybe (IdentI PaymentType)) "payType"
                                 "Тип оплаты"
  , payment_costTranscript       :: F (Maybe Text) "payment_costTranscript"
                                 "Расшифровка стоимости"
  , payment_partnerCost          :: F (Maybe Double) "payment_partnerCost"
                                 "Стоимость со слов партнёра (число)"
  , payment_calculatedCost       :: F (Maybe Int) "payment_calculatedCost"
                                 "Расчётная стоимость"
  , payment_limitedCost          :: F (Maybe Int) "payment_limitedCost"
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
  , bill_billingCost             :: F (Maybe Double) "bill_billingCost"
                                 "Сумма по счёту"
  , bill_billingDate             :: F (Maybe Day) "bill_billingDate"
                                 "Дата выставления счёта"

  , contractor_partner           :: F (Maybe Text) "contractor_partner"
                                 "Партнёр"
  , contractor_partnerId         :: F (Maybe (IdentI Partner)) "contractor_partnerId"
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
  , urgentService                :: F (Maybe (IdentI UrgentServiceReason)) "urgentService"
                                 "Приоритетная услуга"
  , status                       :: F (IdentI ServiceStatus) "status"
                                 "Статус услуги"
  , complication                 :: F (Maybe (IdentI Complication)) "complication"
                                 "Сложный случай"
  , clientSatisfied              :: F (Maybe (IdentI Satisfaction)) "clientSatisfied"
                                 "Клиент доволен"
  , warrantyCase                 :: F (Maybe Checkbox) "warrantyCase"
                                 "Гарантийный случай"
  , files                        :: F (Maybe Reference) "files"
                                 "Прикрепленные файлы"
  }
  deriving Typeable


instance Model Service where
  type TableName Service = "servicetbl"
  modelInfo = mkModelInfo Service ident
  modelView = \case
    "search" -> Just $ modifyView (searchView serviceSearchParams) svcMod
    ""       -> Just $ modifyView defaultView $ usualSvcMod ++ [widget "svcStatus" status]
    _  -> Nothing


svcMod :: [(Text, FieldView -> FieldView) :@ Service]
svcMod =
    [dict contractor_partnerId $ (dictOpt "allPartners")
          { dictType    = Just "ComputedDict"
          , dictBounded = True
          }
    ,setType "dictionary" contractor_partnerId
    ,setMeta "group-widget" "partner" contractor_partner
    ,invisible contractor_coords
    ,hiddenIdent parentId
    , readonly status
    , clientCancelReason `completeWith` CRR.label
    , setMeta "dictionaryLabel" (Aeson.String "realName") creator
    , readonly creator
    ]

-- | Mods that shouldn't be appied to search view
usualSvcMod :: [(Text, FieldView -> FieldView) :@ Service]
usualSvcMod = svcMod ++
            [ invisible contractor_partnerId
            , invisible svcType
            , widget "inline-uploader" files
            , setMeta "reference-widget" "files" files
            , widget "datetime-local" createTime
            , setMeta "group-widget" "group-datetime-local"
                times_expectedServiceStart
            , widget "datetime-local" times_expectedServiceStart
            , widget "datetime-local" times_expectedDispatch
            , widget "datetime-local" times_factServiceStart
            , widget "datetime-local" times_expectedServiceEnd
            , widget "datetime-local" times_factServiceEnd
            , widget "datetime-local" times_expectedServiceClosure
            , widget "datetime-local" times_factServiceClosure
            ]

serviceSearchParams :: [(Text, [Predicate Service])]
serviceSearchParams
  = [("Service_createtime",   interval createTime)
    ,("contractor_partnerId", one contractor_partnerId)
    ,("svcType",              listOf svcType)
    ]
