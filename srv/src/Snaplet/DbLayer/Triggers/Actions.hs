{-# LANGUAGE QuasiQuotes #-}
module Snaplet.DbLayer.Triggers.Actions where

import Prelude hiding (log)

import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import Data.Aeson as Aeson
import Data.String (fromString)

import qualified Fdds as Fdds
------------------------------------------------------------------------------
import WeatherApi (getWeather', tempC)
-----------------------------------------------------------------------------
import Data.Time.Format (parseTime, formatTime)
import Data.Time.Clock (UTCTime, addUTCTime)
import System.Locale (defaultTimeLocale)

import Snap (gets, with)
import Snap.Snaplet.Auth
import Snap.Snaplet.RedisDB
import Snap.Snaplet.Auth
import qualified Database.Redis as Redis
import qualified Snaplet.DbLayer.RedisCRUD as RC
import qualified Snap.Snaplet.PostgresqlSimple as PG
import Database.PostgreSQL.Simple.SqlQQ
import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl
import Snaplet.DbLayer.Triggers.SMS
import Snaplet.DbLayer.Triggers.MailToDealer
import Snaplet.DbLayer.Triggers.MailToPSA

import Snap.Snaplet.SimpleLog

import Util as U
import qualified  Utils.RKCCalc as RKC

services =
  ["deliverCar"
  ,"deliverParts"
  ,"hotel"
  ,"information"
  ,"rent"
  ,"sober"
  ,"taxi"
  ,"tech"
  ,"tech1"
  ,"towage"
  ,"transportation"
  ,"ken"
  ,"bank"
  ,"tickets"
  ,"continue"
  ,"deliverClient"
  ,"averageCommissioner"
  ,"insurance"
  ,"consultation"
  ]

add model field tgs = Map.unionWith (Map.unionWith (++)) $ Map.singleton model (Map.singleton field tgs)

actions :: MonadTrigger m b => Map.Map ModelName (Map.Map FieldName [ObjectId -> FieldValue -> m b ()])
-- actions :: TriggerMap a
actions
    = add "towage" "suburbanMilage" [\objId val -> setSrvMCost objId]
    $ add "tech"   "suburbanMilage" [\objId val -> setSrvMCost objId]
    $ add "rent"   "providedFor"    [\objId val -> setSrvMCost objId]
    $ add "hotel"  "providedFor"    [\objId val -> setSrvMCost objId]
    $ add "towage" "contractor_address" [
      \objId val -> set objId "towerAddress_address" val
      ]
    $ add "towage" "towDealer_address" [
      \objId val -> set objId "towAddress_address" val
      ]
    $ add "towage" "contractor_coords" [
      \objId val -> set objId "towerAddress_coords" val
      ]
    $ add "towage" "towDealer_coords" [
      \objId val -> set objId "towAddress_coords" val
      ]
    $ Map.fromList
      $ [(s,serviceActions) | s <- services]
      ++[("sms", Map.fromList
        [("caseId",   [\smsId _ -> updateSMS smsId])
        ,("template", [\smsId t -> updateSMS smsId])
        ,("msg",      [\smsId _ -> updateSMS smsId])
        ]
      )]
      ++[("action", actionActions)
        ,("cost_serviceTarifOption", Map.fromList
          [("count",
            [\objId val -> do
                case mbreadDouble val of
                  Nothing -> return ()
                  Just v  -> do
                    p <- get objId "price" >>= return . fromMaybe 0 . mbreadDouble
                    set objId "cost" $ printBPrice $ v * p
                    srvId <- get objId "parentId"
                    set srvId "cost_counted" =<< srvCostCounted srvId
            ])
          ])
        ,("case", Map.fromList
          [("caseStatus", [\kazeId st -> case st of
            "s0.5" -> do
              due <- dateNow (+ (1*60))
              actionId <- new "action" $ Map.fromList
                [("name", "tellMeMore")
                ,("duetime", due)
                ,("description", utf8 "Требуется дополнительная обработка кейса")
                ,("targetGroup", "back")
                ,("priority", "1")
                ,("caseId", kazeId)
                ,("closed", "0")
                ]
              upd kazeId "actions" $ addToList actionId
            _      -> return ()])
          ,("partner", [\objId _ -> do
            mapM_ setSrvMCost =<< B.split ',' <$> get objId "services"
            ])
          ,("program", [\objId _ -> do
            mapM_ setSrvMCost =<< B.split ',' <$> get objId "services"
            ])
          -- ,("contact_name",
          --   [\objId val -> set objId "contact_name" $ upCaseStr val])
          -- ,("contact_ownerName", 
          --   [\objId val -> set objId "contact_ownerName" $ upCaseStr val])
          ,("city", [\objId val -> do
                      oldCity <- redisHGet objId "city"
                      case oldCity of
                        Left _         -> return ()
                        Right Nothing  -> setWeather objId val
                        Right (Just c) -> when (c /= val) $ setWeather objId val
                      ])
          ,("car_plateNum", [\objId val ->
            when (B.length val > 5)
              $ set objId "car_plateNum" $ bToUpper val])
          ,("car_vin", [\objId val -> do
            let vin = T.encodeUtf8 . T.toUpper . T.filter isAlphaNum
                    $ T.decodeUtf8 val
            when (B.length vin == 17) $ do
              set objId "car_vin" vin
              fillFromContract vin objId >>= \case
                True -> set objId "vinChecked" "base"
                False -> redisHGetAll (B.concat ["vin:", vin]) >>= \case
                  Left _    -> return ()
                  Right []  -> do
                    res <- requestFddsVin objId val
                    set objId "vinChecked"
                      $ if res then "fdds" else "vinNotFound"
                  Right car -> do
                    set objId "vinChecked" "base"
                    let setIfEmpty (name,val)
                          | name == "car_plateNum" = return ()
                          | otherwise = do
                            val' <- get objId name
                            when (val' == "") $ set objId name val
                    mapM_ setIfEmpty car
            ])
          ,("psaExportNeeded",
            [\caseRef val -> when (val == "1") $ tryRepTowageMail caseRef])
          ])
        ,("contract", Map.fromList
          [("carPlateNum",  [\o -> set o "carPlateNum" . bToUpper])
          ,("carVin",       [\o -> set o "carVin" . bToUpper])
          ,("carCheckPeriod", [setContractValidUntilMilage])
          ,("milageTO",       [setContractValidUntilMilage])
          ,("contractValidFromDate", [setContractValidUntilDate])
          ])
        ]

bToUpper :: ByteString -> ByteString
bToUpper = T.encodeUtf8 . T.toUpper . T.decodeUtf8

fillFromContract :: MonadTrigger m b => ByteString -> ByteString -> m b Bool
fillFromContract vin objId = do
  res <- liftDb $ PG.query (fromString [sql|
    SELECT
      program, carMake, carModel, carPlateNum,
      carCheckPeriod::text,
      extract (epoch from contractValidFromDate)::int8::text,
      extract (epoch from contractValidUntilDate)::int8::text,
      milageTO::text, cardNumber, carMakeYear::text,
      contractValidUntilMilage::text,
      extract (epoch from contractValidFromDate)::int8::text,
      carSeller, carDealerTO
      FROM contracttbl
      WHERE carVin = ?
      ORDER BY ctime DESC LIMIT 1
    |]) [vin]
  case res of
    [] -> return False
    [row] -> do
      zipWithM_ (maybe (return ()) . (set objId))
        ["program", "car_make", "car_model", "car_plateNum", "car_checkPeriod"
        ,"car_serviceStart", "car_serviceEnd","car_checkupMileage"
        ,"cardNumber_cardNumber", "car_makeYear", "cardNumber_validUntilMilage"
        ,"cardNumber_validFrom", "car_seller", "car_dealerTO"]
        row
      return True


serviceActions :: MonadTrigger m b => Map.Map ByteString [ObjectId -> ObjectId -> m b ()]
serviceActions = Map.fromList
  [("status", [\objId val ->
    case val of
      "backoffice" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          -- Check if backoffice transfer is related to callMeMaybe action
          relatedUser <- liftDb $ PG.query (fromString [sql|
            SELECT coalesce(a.assignedTo, '') FROM actiontbl a
              WHERE a.caseId = ?
                AND a.name = 'tellMeMore' AND a.result <> 'communicated'
              LIMIT 1
            |]) [kazeId]
          actionId <- new "action" $ Map.fromList
            [("name", "orderService")
            ,("duetime", due)
            ,("description", utf8 "Заказать услугу")
            ,("targetGroup", "back")
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ,("assignedTo", case relatedUser of { [[u]] -> u; _ -> "" })
            ]
          upd kazeId "actions" $ addToList actionId
          sendSMS actionId "smsTpl:13"
      "recallClient" -> do
          due <- getService objId "times_expectedServiceStart"
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "callMeMaybe")
            ,("duetime", due)
            ,("description", utf8 "Заказ услуги через мобильное приложение")
            ,("targetGroup", "back")
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
          sendSMS actionId "smsTpl:13"
      "serviceOrdered" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          currentUser <- maybe "" userLogin <$> getCurrentUser
          act1 <- new "action" $ Map.fromList
            [("name", "tellClient")
            ,("duetime", due)
            ,("description", utf8 "Сообщить клиенту о договорённости")
            ,("targetGroup", "back")
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("assignedTo", T.encodeUtf8 currentUser)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList act1
          due <- dateNow (+ (14*24*60*60))
          act2 <- new "action" $ Map.fromList
            [("name", "addBill")
            ,("duetime", due)
            ,("description", utf8 "Прикрепить счёт")
            ,("targetGroup", "parguy")
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("assignedTo", "")
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList act2
      "mechanicConf" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "mechanicConf")
            ,("duetime", due)
            ,("description", utf8 "Требуется конференция с механиком")
            ,("targetGroup", "back")
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
          sendSMS actionId "smsTpl:13"
      "dealerConf" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "dealerConf")
            ,("duetime", due)
            ,("description", utf8 "Требуется конференция с дилером")
            ,("targetGroup", "back")
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
          sendSMS actionId "smsTpl:13"
      "pleaseCheck" -> do
          due <- dateNow (+ (5*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "checkStatus")
            ,("duetime", due)
            ,("description",
                utf8 "Клиент попросил уточнить, когда начётся оказание услуги")
            ,("targetGroup", "back")
            ,("priority", "3")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "dealerConformation" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "dealerApproval")
            ,("duetime", due)
            ,("description", utf8 "Требуется согласование с дилером")
            ,("targetGroup", "back")
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "makerConformation" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "carmakerApproval")
            ,("duetime", due)
            ,("description", utf8 "Требуется согласование с заказчиком программы")
            ,("targetGroup", "back")
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "clientCanceled" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "cancelService")
            ,("duetime", due)
            ,("description", utf8 "Клиент отказался от услуги (сообщил об этом оператору Front Office)")
            ,("targetGroup", "back")
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      _ -> return ()]
  )
  ,("clientSatisfied",
    [\objId val ->
        case val of
          "notSatis" -> do
            due <- dateNow (+ (1*60))
            kazeId <- get objId "parentId"
            actionId <- new "action" $ Map.fromList
              [("name", "complaintResolution")
              ,("duetime", due)
              ,("description", utf8 "Клиент предъявил претензию")
              ,("targetGroup", "supervisor")
              ,("priority", "1")
              ,("parentId", objId)
              ,("caseId", kazeId)
              ,("closed", "0")
              ]
            upd kazeId "actions" $ addToList actionId
          _ -> return ()]
  )
  ,("contractor_partner",
    [\objId val -> do
        opts <- get objId "cost_serviceTarifOptions"
        let ids = B.split ',' opts
        redisDel ids
        set objId "cost_serviceTarifOptions" ""
    ])
  ,("falseCall",
    [\objId val -> set objId "cost_counted" =<< srvCostCounted objId])
  ,("contractor_partnerId",
    [\objId val -> do
        srvs <- get val "services" >>= return  . B.split ','
        let m = head $ B.split ':' objId
        s <- filterM (\s -> get s "serviceName" >>= return . (m ==)) srvs
        case s of
          []     -> set objId "falseCallPercent" ""
          (x:xs) -> get x "falseCallPercent" >>= set objId "falseCallPercent"
    ])
  ,("payType",
    [\objId val -> do
        case selectPrice val of
          Nothing       -> set objId "cost_counted" ""
          Just priceSel -> do
            ids <- get objId "cost_serviceTarifOptions" >>=
                         return . B.split ','
            forM_ ids $ \id -> do
              price <- get id priceSel >>= return . fromMaybe 0 . mbreadDouble
              count <- get id "count" >>= return . fromMaybe 0 . mbreadDouble
              set id "price" $  printBPrice price
              set id "cost" =<< printBPrice <$> calcCost id
            srvCostCounted objId >>= set objId "cost_counted"
        ])
  ,("cost_serviceTarifOptions",
    [\objId val -> set objId "cost_counted" =<< srvCostCounted objId ])
   -- RKC calc
  ,("suburbanMilage", [\objId val -> setSrvMCost objId])
  ,("providedFor",    [\objId val -> setSrvMCost objId])
  ,("times_expectedServiceStart",
    [\objId val -> do
      let Just tm = fst <$> B.readInt val
      let h = 3600 -- seconds
      set objId "times_expectedServiceEnd"     $ B.pack $ show $ tm + 1*h
      set objId "times_expectedServiceClosure" $ B.pack $ show $ tm + 11*h
    ])
  ]

resultSet1 =
  ["partnerNotOk", "caseOver", "partnerFound"
  ,"carmakerApproved", "dealerApproved", "needService"
  ]

actionActions :: MonadTrigger m b => Map.Map ByteString [ObjectId -> ByteString -> m b ()]
actionActions = Map.fromList
  [("result",
    [\objId val -> when (val `elem` resultSet1) $ do
         setService objId "status" "orderService"
         void $ replaceAction
             "orderService"
             "Заказать услугу"
             "back" "1" (+5*60) objId

    ,\objId _al -> do
      dateNow id >>= set objId "closeTime"
      Just u <- liftDb $ with auth currentUser
      set objId "assignedTo" $ T.encodeUtf8 $ userLogin u

    ,\objId val -> maybe (return ()) ($objId)
      $ Map.lookup val actionResultMap
    ])
  ,("closed",
    [\objId -> \case
      "1" -> closeAction objId
      "0" -> do
        kazeId <- get objId "caseId"
        upd kazeId "actions" $ addToList objId
    ])
  ]

actionResultMap :: MonadTrigger m b => Map.Map ByteString (ObjectId -> m b ())
actionResultMap = Map.fromList
  [("busyLine",        \objId -> dateNow (+ (5*60))  >>= set objId "duetime" >> set objId "result" "")
  ,("callLater",       \objId -> dateNow (+ (30*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("bigDelay",        \objId -> dateNow (+ (6*60*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("weekDelay",        \objId -> dateNow (+ (7*24*60*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("partnerNotFound", \objId -> dateNow (+ (2*60*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("clientCanceledService", \objId -> closeAction objId >> sendSMS objId "smsTpl:2" >> sendMailToPSA objId)
  ,("unassignPlease",  \objId -> set objId "assignedTo" "" >> set objId "result" "")
  ,("needPartner",     \objId -> do
     setService objId "status" "needPartner"
     newAction <- replaceAction
         "needPartner"
         "Требуется найти партнёра для оказания услуги"
         "parguy" "1" (+60) objId
     set newAction "assignedTo" ""
  )
  ,("serviceOrdered", \objId -> do
    newPartnerMessage objId

    setService objId "status" "serviceOrdered"
    svcId    <- get objId "parentId"
    assignee <- get objId "assignedTo"
    set svcId "assignedTo" assignee

    act <- replaceAction
      "addBill"
      "Прикрепить счёт"
      "parguy" "1" (+14*24*60*60)
      objId
    set act "assignedTo" ""

    sendMailToPSA objId
    isReducedMode >>= \case
      True -> do
        closeSerivceAndSendInfoVW objId
        sendMailToDealer objId
      False -> do
        void $ replaceAction
          "tellClient"
          "Сообщить клиенту о договорённости"
          "back" "1" (+60) objId

  )
  ,("serviceOrderedSMS", \objId -> do
    newPartnerMessage objId
    sendSMS objId "smsTpl:1"

    setService objId "status" "serviceOrdered"
    svcId    <- get objId "parentId"
    assignee <- get objId "assignedTo"
    set svcId "assignedTo" assignee

    sendMailToPSA objId
    isReducedMode >>= \case
      True -> do
        closeSerivceAndSendInfoVW objId
        sendMailToDealer objId
      False -> do
        tm <- getService objId "times_expectedServiceStart"
        void $ replaceAction
          "checkStatus"
          "Уточнить статус оказания услуги"
          "back" "3" (changeTime (+5*60) tm)
          objId
  )
  ,("partnerNotOk", void . replaceAction
      "cancelService"
      "Требуется отказаться от заказанной услуги"
      "back" "1" (+60)
  )
  ,("moveToAnalyst", \objId -> do
    act <- replaceAction
      "orderServiceAnalyst"
      "Заказ услуги аналитиком"
      "analyst" "1" (+60) objId
    set act "assignedTo" ""
  )
  ,("moveToBack", \objId -> do
    act <- replaceAction
      "orderService"
      "Заказ услуги оператором Back Office"
      "back" "1" (+60) objId
    set act "assignedTo" ""
  )
  ,("needPartnerAnalyst",     \objId -> do
     setService objId "status" "needPartner"
     newAction <- replaceAction
         "needPartner"
         "Требуется найти партнёра для оказания услуги"
         "parguy" "1" (+60) objId
     set newAction "assignedTo" ""
  )
  ,("serviceOrderedAnalyst", \objId -> do
    setService objId "status" "serviceOrdered"
    sendMailToPSA objId

    isReducedMode >>= \case
      True -> do
        closeAction objId
        sendMailToDealer objId
      False -> do
        void $ replaceAction
          "tellClient"
          "Сообщить клиенту о договорённости"
          "back" "1" (+60) objId
  )
  ,("dealerNotApproved", \objId ->
    void $ replaceAction
      "tellDealerDenied"
      "Сообщить об отказе дилера"
      "back" "3" (+60) objId
  )
  ,("carmakerNotApproved", \objId ->
    void $ replaceAction
      "tellMakerDenied"
      "Сообщить об отказе автопроизводителя"
      "back" "3" (+60) objId
  )
  ,("partnerNotOkCancel", \objId -> do
      setService objId "status" "cancelService"
      void $ replaceAction
         "cancelService"
         "Требуется отказаться от заказанной услуги"
         "back" "1" (+60) objId
  )
  ,("partnerOk", \objId ->
    isReducedMode >>= \case
      True -> closeAction objId
      False -> do
        tm <- getService objId "times_expectedServiceStart"
        void $ replaceAction
          "checkStatus"
          "Уточнить статус оказания услуги"
          "back" "3" (changeTime (+5*60) tm)
          objId
  )
  ,("serviceDelayed", \objId -> do
    setService objId "status" "serviceDelayed"
    void $ replaceAction
      "tellDelayClient"
      "Сообщить клиенту о задержке начала оказания услуги"
      "back" "1" (+60)
      objId
  )
  ,("serviceInProgress", \objId -> do
    setService objId "status" "serviceInProgress"
    isReducedMode >>= \case
      True -> closeAction objId
      False -> do
        tm <- getService objId "times_expectedServiceEnd"
        void $ replaceAction
          "checkEndOfService"
          "Уточнить у клиента окончено ли оказание услуги"
          "back" "3" (changeTime (+5*60) tm)
          objId
  )
  ,("prescheduleService", \objId -> do
    setService objId "status" "serviceInProgress"
    isReducedMode >>= \case
      True -> closeAction objId
      False -> do
        tm <- getService objId "times_expectedServiceEnd"
        void $ replaceAction
          "checkEndOfService"
          "Уточнить у клиента окончено ли оказание услуги"
          "back" "3" (+60)
          objId
        sendMailToDealer objId
  )
  ,("serviceStillInProgress", \objId ->
    isReducedMode >>= \case
      True -> closeAction objId
      False -> do
        tm <- getService objId "times_expectedServiceEnd"
        dateNow (changeTime (+5*60) tm) >>= set objId "duetime"
        set objId "result" ""
  )
  ,("clientWaiting", \objId -> do
    tm <- getService objId "times_expectedServiceStart"
    void $ replaceAction
      "checkStatus"
      "Уточнить статус оказания услуги"
      "back" "3" (changeTime (+5*60) tm)
      objId
  )
  ,("serviceFinished", \objId -> do
    closeSerivceAndSendInfoVW objId
    sendSMS objId "smsTpl:3"
    sendMailToDealer objId
  )
  ,("complaint", \objId -> do
    closeSerivceAndSendInfoVW objId
    setService objId "clientSatisfied" "notSatis"
    act1 <- replaceAction
      "complaintResolution"
      "Клиент предъявил претензию"
      "supervisor" "1" (+60)
      objId
    set act1 "assignedTo" ""
  )
  ,("billNotReady", \objId -> dateNow (+ (5*24*60*60))  >>= set objId "duetime")
  ,("billAttached", \objId -> do
    act <- replaceAction
      "headCheck"
      "Проверка РКЦ"
      "op_checker" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("parguyToBack", \objId -> do
    act <- replaceAction
      "parguyNeedInfo"
      "Менеджер по Партнёрам запросил доп. информацию"
      "back" "3" (+360) objId
    set act "assignedTo" ""
  )
  ,("backToParyguy", \objId -> do
    act <- replaceAction
      "addBill"
      "Прикрепить счёт"
      "parguy" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("headToParyguy", \objId -> do
    act <- replaceAction
      "addBill"
      "На доработку МпП"
      "parguy" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("confirm", \objId -> do
    act <- replaceAction
      "directorCheck"
      "Проверка директором"
      "director" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("confirmWODirector", \objId -> do
    act <- replaceAction
      "accountCheck"
      "Проверка бухгалтерией"
      "account" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("confirmFinal", \objId -> do
    act <- replaceAction
      "analystCheck"
      "Обработка аналитиком"
      "analyst" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("directorToHead", \objId -> do
    act <- replaceAction
      "headCheck"
      "Проверка РКЦ"
      "op_checker" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("directorConfirm", \objId -> do
    act <- replaceAction
      "accountCheck"
      "Проверка бухгалтерией"
      "account" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("dirConfirmFinal", \objId -> do
    act <- replaceAction
      "analystCheck"
      "Обработка аналитиком"
      "analyst" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("vwclosed", \objId -> do
    sendMailToPSA objId
    closeAction objId
  )
  ,("complaintManaged", closeAction
  )
  ,("communicated", closeAction
  )
  ,("accountConfirm", \objId -> do
    act <- replaceAction
      "analystCheck"
      "Обработка аналитиком"
      "analyst" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("accountToDirector", \objId -> do
    act <- replaceAction
      "directorCheck"
      "Проверка директором"
      "director" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("analystChecked", closeAction)
  ,("caseClosed", \objId -> do
    setService objId "status" "serviceClosed"
    closeAction objId
  )
  ,("partnerGivenCloseTime", \objId -> do
    tm <- getService objId "times_expectedServiceClosure"
    dateNow (changeTime (+5*60) tm) >>= set objId "duetime"
    set objId "result" "")
  ,("falseCallWBill", \objId -> do
     setService objId "falseCall" "bill"
     closeAction objId
     sendSMS objId "smsTpl:2"
  )
  ,("falseCallWOBill", \objId -> do
     setService objId "falseCall" "nobill"
     closeAction objId
     sendSMS objId "smsTpl:2"
  )
  ,("clientNotified", \objId -> do
     setService objId "status" "serviceClosed"
     closeAction objId
  )
  ,("notNeedService", \objId -> do
     setService objId "status" "serviceClosed"
     closeAction objId
  )
  ]

changeTime :: (Int -> Int) -> ByteString -> Int -> Int
changeTime fn x y = case B.readInt x of
  Just (r,"") -> fn r
  _ -> fn y

setService objId field val = do
  svcId <- get objId "parentId"
  set svcId field val

getService objId field
  = get objId "parentId"
  >>= (`get` field)


newPartnerMessage objId = do
  svcId <- get objId "parentId"
  partnerId <- get svcId "contractor_partner"
  kazeId <- get svcId "parentId"
  now <- dateNow id

  addr   <- get kazeId "caseAddress_address"
  name   <- get kazeId "contact_name"
  phone  <- get kazeId "contact_phone1"
  carNum <- get kazeId "car_plateNum"

  let msg = Aeson.object
        ["addr"   .= addr
        ,"name"   .= name
        ,"phone"  .= phone
        ,"carNum" .= carNum
        ]

  void $ new "partnerMessage" $ Map.fromList
    [("ctime", now)
    ,("caseId", kazeId)
    ,("partnerId", partnerId)
    ,("message", L.toStrict $ Aeson.encode msg)
    ]


closeSerivceAndSendInfoVW objId = do
  setService objId "status" "serviceOk"

  partner <- getService objId "contractor_partner"
  comment <- get objId "comment"
  let addParComment act = set act "comment"
        $ B.concat [utf8 "Партнёр: ", partner, "\n\n", comment]

  tm <- getService objId "times_expectedServiceClosure"
  act1 <- replaceAction
    "closeCase"
    "Закрыть заявку"
    "op_close" "3" (changeTime (+7*24*60*60) tm)
    objId
  set act1 "assignedTo" ""
  addParComment act1

  program <- get objId "caseId" >>= (`get` "program")
  when (program `elem` ["vwMotor", "vwcargo", "peugeot", "citroen"]) $ do
    act2 <- replaceAction
      "getInfoDealerVW"
      "Уточнить информацию о ремонте у дилера/партнёра (VW, PSA)"
      "op_dealer" "3" (+7*24*60*60)
      objId
    set act2 "assignedTo" ""
    addParComment act2



closeAction objId = do
  kazeId <- get objId "caseId"
  upd kazeId "actions" $ dropFromList objId
  set objId "closed" "1"

replaceAction actionName actionDesc targetGroup priority dueDelta objId = do
  assignee <- get objId "assignedTo"
  svcId <- get objId "parentId"
  due <- dateNow dueDelta
  kazeId <- get svcId "parentId"
  actionId <- new "action" $ Map.fromList
    [("name", actionName)
    ,("description", utf8 actionDesc)
    ,("targetGroup", targetGroup)
    ,("assignedTo", assignee)
    ,("priority", priority)
    ,("duetime", due)
    ,("parentId", svcId)
    ,("caseId", kazeId)
    ,("closed", "0")
    ]
  upd kazeId "actions" $ addToList actionId
  closeAction objId
  return actionId

requestFddsVin :: MonadTrigger m b => B.ByteString -> B.ByteString -> m b Bool
requestFddsVin objId vin = do
  let preparedVin = B.unpack $ B.map toUpper vin
  conf     <- liftDb $ gets fdds
  vinState <- liftIO Fdds.vinSearchInit
  result   <- liftIO (try $ Fdds.vinSearch conf vinState preparedVin
                      :: IO (Either SomeException [Fdds.Result]))
  case result of
    Right v -> return $ any (Fdds.rValid) v
    Left _  -> return False

setWeather :: MonadTrigger m b => B.ByteString -> B.ByteString -> m b ()
setWeather objId city = do
  conf    <- liftDb $ gets weather
  weather <- liftIO $ getWeather' conf $ U.bToString $ B.filter (/= '\'') city
  case weather of
    Right w   -> do
      liftDb $ scope "weather" $ log Trace $ T.concat
        [ "got for: ", T.decodeUtf8 objId
        , "; city: " , T.decodeUtf8 city
        , "; weather: ", T.pack $ show w
        ]
      set objId "temperature" $ B.pack $ show $ tempC w
    Left  err -> do
      set objId "temperature" ""
      liftDb $ scope "weather" $ log Debug $ T.concat
        [ "can't retrieve for: ", T.decodeUtf8 objId
        , "; city: " , T.decodeUtf8 city
        , "; error: ", T.pack $ show err
        ]
      return ()

srvCostCounted srvId = do
  falseCall        <- get srvId "falseCall"
  falseCallPercent <- get srvId "falseCallPercent" >>=
                      return . fromMaybe 100 . mbreadDouble
  tarifIds <- get srvId "cost_serviceTarifOptions" >>= return . B.split ','
  cost <- sum <$> mapM calcCost tarifIds
  case falseCall of
    "bill" -> return $ printBPrice $ cost * (falseCallPercent / 100)
    _      -> return $ printBPrice cost

calcCost id = do
  p <- get id "price" >>= return . fromMaybe 0 . mbreadDouble
  c <- get id "count" >>= return . fromMaybe 0 . mbreadDouble
  return $ p * c

setSrvMCost :: MonadTrigger m b => B.ByteString -> m b ()
setSrvMCost id = do
  obj    <- readObject id
  parent <- readObject $ fromJust $ Map.lookup "parentId" obj
  dict   <- liftDb $ gets rkcDict
  set id "marginalCost" $ RKC.setSrvMCost srvName obj parent dict
    where
      -- readR   = lift . RC.read' redis
      srvName = head $ B.split ':' id

setContractValidUntilMilage :: MonadTrigger m b =>
                               B.ByteString -> B.ByteString -> m b ()
setContractValidUntilMilage obj _ = do
  v      <- get obj "contractValidUntilMilage"
  milage <- mbreadInt <$> get obj "milageTO"
  p   <- get obj "program"
  per <- mbreadInt <$> get (B.concat ["program:", p]) "carCheckPeriodDefault"
  case (v, per, milage) of
    ("", Just c, Just m) -> setMillage $ B.pack $ show $ c + m
    _ -> return ()
    where setMillage = set obj "contractValidUntilMilage"

setContractValidUntilDate ::  MonadTrigger m b =>
                              B.ByteString -> B.ByteString -> m b ()
setContractValidUntilDate obj val = do
  let d = parseTime defaultTimeLocale "%s" $ B.unpack val :: Maybe UTCTime
  v   <- get obj "contractValidUntilDate"
  p   <- get obj "program"
  due <- mbreadInt <$> get (B.concat ["program:", p]) "duedateDefault"
  case (v, d, due) of
    ("", Just d', Just due') -> setUntilDate $ addUTCTime (d2s due') d'
    _ -> return ()
  where
    d2s d = (fromIntegral d) * 24 * 60 * 60
    setUntilDate =
      set obj "contractValidUntilDate" .
      B.pack .
      formatTime defaultTimeLocale "%s"
