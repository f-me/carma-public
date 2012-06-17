

module Snaplet.DbLayer.Triggers.Actions where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
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
  ++[("action", actionActions)]


serviceActions = Map.fromList
  [("status", [\objId val ->
    case val of
      "backoffice" -> do
          due <- dateNow (+ (5*60))
          actionId <- new "action" $ Map.fromList
            [("name", "orderService")
            ,("duetime", due)
            ,("description", utf8 "Заказать услугу")
            ,("targetGroup", "back")
            ,("priority", "2")
            ,("parentId", objId)
            ,("closed", "false")
            ]
          kazeId <- get objId "parentId"
          upd kazeId "actions" $ addToList actionId
      _ -> return ()]
  )]


resultSet1 =
  ["partnerNotOk", "caseOver", "partnerFound"
  ,"carmakerApproved", "dealerApproved", "needService"
  ] 

actionActions = Map.fromList
  [("result",
    [\objId val -> when (val `elem` resultSet1)
      $ replaceAction
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
  ,("partnerNotFound", \objId -> dateNow (+ (60*60)) >>= set objId "duetime")
  ,("searchPartner",   replaceAction
      "needPartner"
      "Требуется найти партнёра для оказания услуги"
      "parguy" "1" (+60)
  )
  ,("serviceOrdered", replaceAction
      "tellClient"
      "Сообщить клиенту о договорённости" 
      "back" "1" (+60)
  )
  ,("partnerNotOk", replaceAction
      "cancelService"
      "Требуется отказаться от заказанной услуги"
      "back" "1" (+60)
  )
  ,("partnerNotOkCancel", replaceAction
      "cancelService"
      "Требуется отказаться от заказанной услуги"
      "back" "1" (+60)
  )
  ,("falseCallWBill", \objId -> do
    setService objId "falseCall" "bill"
    closeAction objId
  )
  ,("falseCallWOBill", \objId -> do
    setService objId "falseCall" "nobill"
    closeAction objId
  )
  ]
 

setService objId field val = do
  svcId <- get objId "parentId"
  set svcId field val
  

closeAction objId = do
  svcId <- get objId "parentId"
  kazeId <- get svcId "parentId"
  upd kazeId "actions" $ dropFromList objId
  set objId "closed" "true"

replaceAction actionName actionDesc targetGroup priority dueDelta objId = do
  svcId <- get objId "parentId"
  due <- dateNow dueDelta
  actionId <- new "action" $ Map.fromList
    [("name", actionName)
    ,("description", utf8 actionDesc)
    ,("targetGroup", targetGroup)
    ,("priority", priority)
    ,("duetime", due)
    ,("parentId", svcId)
    ,("closed", "false")
    ]
  kazeId <- get svcId "parentId"
  upd kazeId "actions" $ addToList actionId
  closeAction objId

