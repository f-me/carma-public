module Snaplet.DbLayer.Triggers.Actions where

import Control.Arrow (first)
import Control.Monad (when,void)
import Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

import Snap.Snaplet.RedisDB
import qualified Database.Redis as Redis
import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl

services =
  ["deliverCar"
  ,"deliverParts"
  ,"hotel"
  ,"information"
  ,"rent"
  ,"sober"
  ,"taxi"
  ,"tech"
  ,"towage"
  ,"transportation"
  ]

actions :: TriggerMap a
actions = Map.fromList
  $ [(s,serviceActions) | s <- services]
  ++[("action", actionActions)
    ,("case", Map.fromList
      [("car_vin", [\objId val ->
        if B.length val /= 17
          then return ()
          else do
            let vinKey = B.concat ["vin:", val]
            Right car <- lift $ runRedisDB redis
                              $ Redis.hgetall vinKey
            let car' = map (first $ B.append "car_") car
            mapM_ (uncurry $ set objId) car']
      )]
    )]

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
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "false")
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
            ,("closed", "false")
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
            ,("closed", "false")
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
            ,("closed", "false")
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
            ,("closed", "false")
            ]
          upd kazeId "actions" $ addToList actionId
      "clientCanceled" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "cancelService")
            ,("duetime", due)
            ,("description", utf8 "Клиент отказался от услуги (сообщил об это оператору Front Office)")
            ,("targetGroup", "back")
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "false")
            ]
          upd kazeId "actions" $ addToList actionId             
      _ -> return ()]
  )]


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
             "back" "2" (+5*60) objId
    ,\objId val -> maybe (return ()) ($objId)
      $ Map.lookup val actionResultMap
    ]
  )]

actionResultMap = Map.fromList
  [("busyLine",        \objId -> dateNow (+ (5*60))  >>= set objId "duetime")
  ,("callLater",       \objId -> dateNow (+ (30*60)) >>= set objId "duetime")
  ,("partnerNotFound", \objId -> dateNow (+ (2*60*60)) >>= set objId "duetime")
  ,("clientCanceledService", closeAction)   
  ,("unassignPlease",  \objId -> set objId "assignedTo" "")
  ,("needPartner",     \objId -> do 
     setService objId "status" "needPartner"
     newAction <- replaceAction
         "needPartner"
         "Требуется найти партнёра для оказания услуги"
         "parguy" "1" (+60) objId
     set newAction "assignedTo" ""
  )
  ,("serviceOrdered", \objId -> do
     setService objId "status" "serviceOrdered"
     void $ replaceAction
         "tellClient"
         "Сообщить клиенту о договорённости" 
         "back" "1" (+60) objId
  )
  ,("partnerNotOk", void . replaceAction
      "cancelService"
      "Требуется отказаться от заказанной услуги"
      "back" "1" (+60)
  )
  ,("partnerNotOkCancel", \objId -> do
      setService objId "status" "cancelService"
      void $ replaceAction
         "cancelService"
         "Требуется отказаться от заказанной услуги"
         "back" "1" (+60) objId
  )
  ,("partnerOk", \objId -> do
    tm <- getService objId "times_expectedServiceStart"
    void $ replaceAction
      "checkStatus"
      "Уточнить статус оказания услуги"
      "back" "1" (changeTime (+5*60) tm)
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
    tm <- getService objId "times_expectedServiceEnd"
    void $ replaceAction
      "checkEndOfService"
      "Уточнить у клиента окончено ли оказание услуги"
      "back" "1" (changeTime (+5*60) tm)
      objId
  )  
  ,("serviceStillInProgress", \objId -> do
    tm <- getService objId "times_expectedServiceEnd"  
    dateNow (changeTime (+5*60) tm) >>= set objId "duetime") 
  ,("clientWaiting", \objId -> do
    tm <- getService objId "times_expectedServiceStart"
    void $ replaceAction
      "checkStatus"
      "Уточнить статус оказания услуги"
      "back" "1" (changeTime (+5*60) tm)
      objId
  )
  ,("serviceFinished", \objId -> do
    setService objId "status" "serviceOk"
    tm <- getService objId "times_expectedServiceClosure"	
    void $ replaceAction
      "closeCase"
      "Закрыть заявку"
      "back" "1" (changeTime (+5*60) tm)
      objId
    act <- replaceAction
      "addBill"
      "Прикрепить счёт"
      "parguy" "1" (+14*24*60*60)
      objId
    set act "assignedTo" ""
    void $ replaceAction
      "getInfoDealerVW"
      "Требуется уточнить информацию о ремонте у дилера (только для VW)"
      "back" "1" (+7*24*60*60)
      objId
  )
  ,("complaint", \objId -> do
    setService objId "status" "serviceOk"
    setService objId "clientSatisfied" "0"
    tm <- getService objId "times_expectedServiceClosure"		
    act1 <- replaceAction
      "complaintResolution"
      "Клиент предъявил претензию"
      "head" "1" (+60)
      objId 
    set act1 "assignedTo" ""
    void $ replaceAction
      "closeCase"
      "Закрыть заявку"
      "back" "1" (changeTime (+5*60) tm)
      objId
    act2 <- replaceAction
      "addBill"
      "Прикрепить счёт"
      "parguy" "1" (+14*24*60*60)
      objId
    set act2 "assignedTo" ""
    void $ replaceAction
      "getInfoDealerVW"
      "Требуется уточнить информацию о ремонте у дилера (только для VW)"
      "back" "1" (+7*24*60*60)
      objId
  )
  ,("billNotReady", \objId -> dateNow (+ (5*24*60*60))  >>= set objId "duetime")
  ,("billAttached", \objId -> do
    act <- replaceAction
      "accountCheck"
      "Проверить кейс"
      "account" "1" (+60) objId
    set act "assignedTo" ""
  )
  ,("vwclosed", closeAction
  )   
  ,("financialOk", closeAction
  )
  ,("accountError", void . replaceAction
      "editCaseAfterClosure"
      "Редактирование кейса после выставления счёта"
      "back" "1" (+60)
  )
  ,("caseEdited", closeAction
  )
  ,("recloseService", void . replaceAction
      "closeCase"
      "Закрыть заявку"
      "back" "1" (+60)
  )
  ,("caseClosedFinancialNotOk", \objId -> do
    setService objId "status" "serviceClosed"
    void $ replaceAction
      "financialClose"
      "Заявка закрыта, требуется финансовая информация"
      "back" "1" (+60)
      objId
    void $ replaceAction
      "caseContinue"
      "Узнать у клиента требуются ли ему ещё услуги?"
      "back" "1" (+60)
      objId   
  )
  ,("caseClosedFinancialOk", \objId -> do
    setService objId "status" "serviceClosed"
    closeAction objId	
  )
  ,("financialOk", \objId -> do
     setService objId "status" "serviceClosed"
     closeAction objId
  )    
  ,("falseCallWBill", \objId -> do
     setService objId "falseCall" "bill"
     closeAction objId
  )
  ,("falseCallWOBill", \objId -> do
     setService objId "falseCall" "nobill"
     closeAction objId
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
  

closeAction objId = do
  svcId <- get objId "parentId"
  kazeId <- get svcId "parentId"
  upd kazeId "actions" $ dropFromList objId
  set objId "closed" "true"

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
    ,("closed", "false")
    ]
  upd kazeId "actions" $ addToList actionId
  closeAction objId
  return actionId
