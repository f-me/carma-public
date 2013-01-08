module Snaplet.DbLayer.Triggers.Actions where

import Prelude hiding (log)

import Control.Arrow (first)
import Control.Monad (when, unless, void, forM, forM_, filterM)
import Control.Monad.Trans
import Control.Exception
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8  as BU
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import Data.Aeson as Aeson

import qualified Fdds as Fdds
------------------------------------------------------------------------------
import WeatherApi (getWeather', tempC)
-----------------------------------------------------------------------------
import Data.Time.Format (parseTime)
import Data.Time.Clock (UTCTime)
import System.Locale (defaultTimeLocale)

import Snap (gets, with)
import Snap.Snaplet.RedisDB
import qualified Database.Redis as Redis
import qualified Snaplet.DbLayer.RedisCRUD as RC
import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl
import Snaplet.DbLayer.Triggers.SMS

import Snap.Snaplet.SimpleLog

import Util
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

actions :: TriggerMap a
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
          [("partner", [\objId val -> do
            mapM_ (setSrvMCost) =<< B.split ',' <$> get objId "services"
            return ()
                       ])
          ,("program", [\objId val -> do
            mapM_ (setSrvMCost) =<< B.split ',' <$> get objId "services"
            return ()
                       ])
          -- ,("contact_name",
          --   [\objId val -> set objId "contact_name" $ upCaseStr val])
          -- ,("contact_ownerName", 
          --   [\objId val -> set objId "contact_ownerName" $ upCaseStr val])
          ,("city", [\objId val -> do
                      oldCity <- lift $ runRedisDB redis $ Redis.hget objId "city"
                      case oldCity of
                        Left _         -> return ()
                        Right Nothing  -> setWeather objId val
                        Right (Just c) -> when (c /= val) $ setWeather objId val
                      ])
          ,("car_vin", [\objId val ->
            when (B.length val == 17) $ do
              let vinKey = B.concat ["vin:", B.map toUpper val]
              car <- lift $ runRedisDB redis
                          $ Redis.hgetall vinKey
              case car of
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
          ])
        ]


-- Создания действий "с нуля"
serviceActions = Map.fromList
  [("status", [\objId val ->
    case val of
      "backoffice" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "orderService")
            ,("duetime", due)
            ,("description", utf8 "Заказать услугу")
            ,("targetGroup", "back")
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
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
        lift $ runRedisDB redis $ Redis.del ids
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

actionActions = Map.fromList
  [("result",
    [\objId val -> when (val `elem` resultSet1) $ do
         setService objId "status" "orderService"
         void $ replaceAction
             "orderService"
             "Заказать услугу"
             "back" "1" (+5*60) objId

    ,\objId _al -> dateNow id >>= set objId "closeTime"
    ,\objId val -> maybe (return ()) ($objId)
      $ Map.lookup val actionResultMap
    ])
  ,("closed",
    [\objId val -> when (val == "1") $ closeAction objId
    ])
  ]

actionResultMap = Map.fromList
  [("busyLine",        \objId -> dateNow (+ (5*60))  >>= set objId "duetime" >> set objId "result" "")
  ,("callLater",       \objId -> dateNow (+ (30*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("bigDelay",        \objId -> dateNow (+ (6*60*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("weekDelay",        \objId -> dateNow (+ (7*24*60*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("partnerNotFound", \objId -> dateNow (+ (2*60*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("clientCanceledService", \objId -> closeAction objId >> sendSMS objId "smsTpl:2")
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

    isReducedMode >>= \case
      True -> closeSerivceAndSendInfoVW objId
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

    isReducedMode >>= \case
      True -> closeSerivceAndSendInfoVW objId
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
    isReducedMode >>= \case
      True -> closeAction objId
      False -> do
        void $ replaceAction
          "tellClient"
          "Сообщить клиенту о договорённости"
          "back" "1" (+60) objId
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
  )
  ,("complaint", \objId -> do
    closeSerivceAndSendInfoVW objId
    setService objId "clientSatisfied" "0"
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
      "supervisor" "1" (+360) objId
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
      "supervisor" "1" (+360) objId
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
  ,("vwclosed", closeAction
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
  tm <- getService objId "times_expectedServiceClosure"
  act1 <- replaceAction
    "closeCase"
    "Закрыть заявку"
    "back" "3" (changeTime (+7*24*60*60) tm)
    objId

  act2 <- replaceAction
    "getInfoDealerVW"
    "Уточнить информацию о ремонте у дилера/партнёра (VW, PSA)"
    "back" "3" (+7*24*60*60)
    objId

  partner <- getService objId "contractor_partner"
  comment <- get objId "comment"
  let comment' = B.concat [utf8 "Партнёр: ", partner, "\n\n", comment]
  mapM_ (\act -> set act "comment" comment') [act1, act2]


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

requestFddsVin :: B.ByteString -> B.ByteString -> TriggerMonad b Bool
requestFddsVin objId vin = do
  let preparedVin = B.unpack $ B.map toUpper vin
  conf     <- lift $ gets fdds
  vinState <- liftIO Fdds.vinSearchInit
  result   <- liftIO (try $ Fdds.vinSearch conf vinState preparedVin
                      :: IO (Either SomeException [Fdds.Result]))
  case result of
    Right v -> return $ any (Fdds.rValid) v
    Left _  -> return False

setWeather objId city = do
  conf    <- lift $ gets weather
  weather <- liftIO $ getWeather' conf $ BU.toString city
  case weather of
    Right w   -> do
      lift $ scope "weather" $ log Trace $ T.concat
        [ "got for: ", T.decodeUtf8 objId
        , "; city: " , T.decodeUtf8 city
        , "; weather: ", T.pack $ show w
        ]
      set objId "temperature" $ B.pack $ show $ tempC w
    Left  err -> do
      set objId "temperature" ""
      lift $ scope "weather" $ log Debug $ T.concat
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

setSrvMCost id = do
  obj    <- readR id
  parent <- readR $ fromJust $ Map.lookup "parentId" obj
  dict   <- lift $ gets rkcDict
  set id "marginalCost" $ RKC.setSrvMCost srvName obj parent dict
    where
      readR   = lift . RC.read' redis
      srvName = head $ B.split ':' id
