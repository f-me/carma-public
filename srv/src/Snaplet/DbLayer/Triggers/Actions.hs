{-# LANGUAGE QuasiQuotes #-}
module Snaplet.DbLayer.Triggers.Actions where

import Prelude hiding (log)

import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import Data.Char
import Data.List (intercalate)
import Data.Maybe
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
import qualified Snap.Snaplet.PostgresqlSimple as PG
import Snap.Snaplet.PostgresqlSimple ((:.)(..), Only(..))
import Database.PostgreSQL.Simple.SqlQQ
import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl
import Snaplet.DbLayer.Triggers.SMS
import Snaplet.DbLayer.Triggers.MailToDealer
import Snaplet.DbLayer.Triggers.MailToPSA
import Snaplet.DbLayer.Triggers.MailToGenser

import Snap.Snaplet.SimpleLog

import Carma.HTTP (read1Reference)


import Data.Model
import qualified Carma.Model.Case as Case
import qualified Carma.Model.Contract as Contract
import qualified Carma.Model.Program as Program
import qualified Carma.Model.SubProgram as SubProgram
import qualified Carma.Model.Role as Role
import qualified Carma.Model.SmsTemplate as SmsTemplate

import Util as U
import qualified  Utils.RKCCalc as RKC


services :: [ModelName]
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

add :: (Ord k1, Ord k2) =>
       k1 -> k2 -> [a]
    -> Map.Map k1 (Map.Map k2 [a])
    -> Map.Map k1 (Map.Map k2 [a])
add model field tgs = Map.unionWith (Map.unionWith (++)) $ Map.singleton model (Map.singleton field tgs)

actions :: MonadTrigger m b => Map.Map ModelName (Map.Map FieldName [ObjectId -> FieldValue -> m b ()])
-- actions :: TriggerMap a
actions
    = add "towage" "suburbanMilage" [\objId _ -> setSrvMCost objId]
    $ add "tech"   "suburbanMilage" [\objId _ -> setSrvMCost objId]
    $ add "rent"   "providedFor"    [\objId _ -> setSrvMCost objId]
    $ add "hotel"  "providedFor"    [\objId _ -> setSrvMCost objId]
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
        ,("template", [\smsId _ -> updateSMS smsId])
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
              now <- dateNow Prelude.id
              due <- dateNow (+ (1*60))
              actionId <- new "action" $ Map.fromList
                [("name", "tellMeMore")
                ,("ctime", now)
                ,("duetime", due)
                ,("description", utf8 "Требуется дополнительная обработка кейса")
                ,("targetGroup", identFv Role.bo_order)
                ,("priority", "1")
                ,("caseId", kazeId)
                ,("closed", "0")
                ]
              upd kazeId "actions" $ addToList actionId
            _      -> return ()])
          ,("services", [\caseId _ -> updateCaseStatus caseId])
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
          ,("contract", [\objId val -> void $ fillFromContract val objId])
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


-- | Mapping between contract and case fields.
--
-- TODO Describe joins.
contractToCase :: [(FA Contract.Contract, FA Case.Case)]
contractToCase =
    [ (FA Contract.name, FA Case.contact_name)
    , (FA Contract.vin, FA Case.car_vin)
    , (FA Contract.make, FA Case.car_make)
    , (FA Contract.model, FA Case.car_model)
    , (FA Contract.seller, FA Case.car_seller)
    , (FA Contract.plateNum, FA Case.car_plateNum)
    , (FA Contract.makeYear, FA Case.car_makeYear)
    , (FA Contract.color, FA Case.car_color)
    , (FA Contract.buyDate, FA Case.car_buyDate)
    , (FA Contract.lastCheckDealer, FA Case.car_dealerTO)
    , (FA Contract.transmission, FA Case.car_transmission)
    , (FA Contract.engineType, FA Case.car_engine)
    , (FA Contract.engineVolume, FA Case.car_liters)
    , (FA Contract.carClass, FA Case.car_class)
    , (FA Contract.subprogram, FA Case.subprogram)
    ]


fillFromContract :: MonadTrigger m b => ByteString -> ByteString -> m b Bool
fillFromContract contract objId = do
  let cid :: IdentI Contract.Contract
      cid = maybe (error "Could not read contract id") (Ident . fst) $
            B.readInt contract
  res <- liftDb $ PG.query
         (fromString $ concat
          [ "SELECT "
          , intercalate "," $ map (const "?::text") contractToCase
          , " FROM \"?\" WHERE id = ?;"
          ]) $
         map (PT . fieldNameE . fst) contractToCase :.
        (Only $ PT $ tableName (modelInfo :: ModelInfo Contract.Contract)) :.
        (Only cid)
  case res of
    [] -> return False
    [row] -> do
      -- Replace only empty fields of case
      let setIfEmpty oid nm val = get oid nm >>= \case
              "" -> set objId nm val
              _  -> return ()
      zipWithM_ (maybe (return ()) . (setIfEmpty objId))
                (map (T.encodeUtf8 . fieldNameE . snd) contractToCase)
                row
      return True
    _ -> error "fillFromContract: Contract primary key is broken"


-- | This is called when service status is changed in some trigger.
onRecursiveServiceStatusChange
  :: MonadTrigger m b => ByteString -> ByteString -> m b ()
onRecursiveServiceStatusChange svcId val = do
  caseId  <- get svcId "parentId"

  -- Check if we need to send message to Genser
  payType <- get svcId "payType"
  pgm     <- get caseId "program"
  let (svc:_) = B.split ':' svcId
  when (svc == "towage"
      && pgm == identFv Program.genser
      && payType == "ruamc"
      && val `elem`
        ["serviceOrdered", "serviceOk"
        ,"cancelService", "clientCanceled"])
    $ sendMailToGenser svcId

  updateCaseStatus caseId


-- | Automatically change case status according to statuses
-- of the contained services.
updateCaseStatus :: MonadTrigger m b => ByteString -> m b ()
updateCaseStatus caseId =
  set caseId "caseStatus" =<< do
    servs <- B.split ',' <$> get caseId "services"
    statuses <- mapM (`get` "status") servs
    return $ case statuses of
      _ | all (`elem` ["serviceClosed","falseCall","mistake"]) statuses
          -> "s2" -- closed
        | all (`elem` ["clientCanceled","serviceClosed"]) statuses
          -> "s2" -- closed
        | all (`elem` ["clientCanceled", "cancelService"]) statuses
          -> "s3" -- cancel
        | any (== "creating") statuses
          -> "s0" -- Front Office
        | otherwise -> "s1" -- Back Office


-- | Clear assignee of control-class action chain head (which has
-- `bo_control` in `targetGroup`) unless the user has both
-- `bo_control` and `bo_order` roles. This will enable the action to
-- be pulled from action pool by bo_control users.
tryToPassChainToControl :: MonadTrigger m b =>
                           AuthUser -> ObjectId -> m b ()
tryToPassChainToControl user action =
    when (not
          (elem (Role $ identFv Role.bo_control) (userRoles user) &&
           elem (Role $ identFv Role.bo_order) (userRoles user))) $
    clearAssignee action

-- | Clear assignee and assignTime of an action.
clearAssignee :: MonadTrigger m b => ObjectId -> m b ()
clearAssignee action = set action "assignedTo" "" >> set action "assignTime" ""

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
                AND a.name IN ('tellMeMore', 'callMeMaybe')
                AND coalesce(a.result, '') <> 'communicated'
              LIMIT 1
            |]) [kazeId]
          now <- dateNow id
          let (assignee, assignTime) =
                  case relatedUser of
                    [[u]] -> (u, now)
                    _     -> ("", "")
          actionId <- new "action" $ Map.fromList
            [("name", "orderService")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", utf8 "Заказать услугу")
            ,("targetGroup", identFv Role.bo_order)
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ,("assignTime", assignTime)
            ,("assignedTo", assignee)
            ]
          upd kazeId "actions" $ addToList actionId
          sendSMS actionId SmsTemplate.create
      "recallClient" -> do
          now <- dateNow id
          due <- dateNow (+ (15*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "tellMeMore")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", utf8  "Заказ услуги (требуется дополнительная информация)")
            ,("targetGroup", identFv Role.bo_order)
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
          sendSMS actionId SmsTemplate.create
      "serviceOrdered" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          Just u <- liftDb $ with auth currentUser
          now <- dateNow id
          act1 <- new "action" $ Map.fromList
            [("name", "tellClient")
            ,("ctime", now)
            ,("duetime", due)
            ,("assignTime", now)
            ,("description", utf8 "Сообщить клиенту о договорённости")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          tryToPassChainToControl u act1

          upd kazeId "actions" $ addToList act1
          now2 <- dateNow id
          due2 <- dateNow (+ (14*24*60*60))
          act2 <- new "action" $ Map.fromList
            [("name", "addBill")
            ,("ctime", now2)
            ,("duetime", due2)
            ,("description", utf8 "Прикрепить счёт")
            ,("targetGroup", identFv Role.bo_bill)
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("assignedTo", "")
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList act2
      "mechanicConf" -> do
          now <- dateNow id
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "mechanicConf")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", utf8 "Требуется конференция с механиком")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
          sendSMS actionId SmsTemplate.create
      "dealerConf" -> do
          now <- dateNow id
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "dealerConf")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", utf8 "Требуется конференция с дилером")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
          sendSMS actionId SmsTemplate.create
      "pleaseCheck" -> do
          now <- dateNow id
          due <- dateNow (+ (5*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "checkStatus")
            ,("ctime", now)
            ,("duetime", due)
            ,("description",
                utf8 "Клиент попросил уточнить, когда начнётся оказание услуги")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "3")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "dealerConformation" -> do
          now <- dateNow id
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "dealerApproval")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", utf8 "Требуется согласование с дилером")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "makerConformation" -> do
          now <- dateNow id
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "carmakerApproval")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", utf8 "Требуется согласование с заказчиком программы")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "clientCanceled" -> do
          now <- dateNow id
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "cancelService")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", utf8 "Клиент отказался от услуги (сообщил об этом оператору Front Office)")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      _ -> return ()
    -- Another one service status trigger.
    -- Sets corresponding case status.
    ,\objId val -> do
      set objId "status" val -- push change to the commit stack
      onRecursiveServiceStatusChange objId val
    ]
  )
  ,("clientSatisfied",
    [\objId val ->
        case val of
          "notSatis" -> do
            now <- dateNow id
            due <- dateNow (+ (1*60))
            kazeId <- get objId "parentId"
            actionId <- new "action" $ Map.fromList
              [("name", "complaintResolution")
              ,("ctime", now)
              ,("duetime", due)
              ,("description", utf8 "Клиент предъявил претензию")
              ,("targetGroup", identFv Role.bo_qa)
              ,("priority", "1")
              ,("parentId", objId)
              ,("caseId", kazeId)
              ,("closed", "0")
              ]
            upd kazeId "actions" $ addToList actionId
          _ -> return ()]
  )
  ,("contractor_partner",
    [\objId _ -> do
        opts <- get objId "cost_serviceTarifOptions"
        let ids = B.split ',' opts
        redisDel ids >> set objId "cost_serviceTarifOptions" ""
    ])
  ,("falseCall",
    [\objId _ -> set objId "cost_counted" =<< srvCostCounted objId])
  ,("contractor_partnerId",
    [\objId val -> do
        srvs <- get val "services" >>= return  . B.split ','
        let m = head $ B.split ':' objId
        s <- filterM (\s -> get s "serviceName" >>= return . (m ==)) srvs
        case s of
          []     -> set objId "falseCallPercent" ""
          (x:_) -> get x "falseCallPercent" >>= set objId "falseCallPercent"
    ])
  ,("payType",
    [\objId val -> do
        case selectPrice val of
          Nothing       -> set objId "cost_counted" ""
          Just priceSel -> do
            ids <- get objId "cost_serviceTarifOptions" >>=
                         return . B.split ','
            forM_ ids $ \oid -> do
              price <- get oid priceSel >>= return . fromMaybe 0 . mbreadDouble
              set oid "price" $  printBPrice price
              set oid "cost" =<< printBPrice <$> calcCost oid
            srvCostCounted objId >>= set objId "cost_counted"
        ])
  ,("cost_serviceTarifOptions",
    [\objId _ -> set objId "cost_counted" =<< srvCostCounted objId ])
   -- RKC calc
  ,("suburbanMilage", [\objId _ -> setSrvMCost objId])
  ,("providedFor",    [\objId _ -> setSrvMCost objId])
  ,("times_expectedServiceStart",
    [\objId val -> do
      let Just tm = fst <$> B.readInt val
      let h = 3600 -- seconds
      set objId "times_expectedServiceEnd"     $ B.pack $ show $ tm + 1*h
      set objId "times_expectedServiceClosure" $ B.pack $ show $ tm + 11*h
      set objId "times_factServiceStart" ""
    ])
  ,("times_expectedDispatch",
    [\objId _ -> set objId "times_factServiceStart" ""
    ])
  ,("times_expectedServiceEnd",
    [\objId _ -> set objId "times_factServiceEnd" ""
    ])
  ,("times_expectedDealerInfo",
    [\objId _ -> set objId "times_factDealerInfo" ""
    ])
  ,("times_expectedServiceClosure",
    [\objId _ -> set objId "times_factServiceClosure" ""
    ])
  ]

resultSet1 :: [FieldValue]
resultSet1 =
  ["partnerNotOk", "caseOver", "partnerFound"
  ,"carmakerApproved", "dealerApproved", "needService"
  ]

actionActions :: MonadTrigger m b => Map.Map ByteString [ObjectId -> ByteString -> m b ()]
actionActions = Map.fromList
  [("result",
    [\objId val -> when (val `elem` resultSet1) $ do
         setServiceStatus objId "orderService"
         void $ replaceAction
             "orderService"
             "Заказать услугу"
             (identFv Role.bo_order) "1" (+5*60) objId

    ,\objId _al -> do
      dateNow id >>= set objId "closeTime"
      Just u <- liftDb $ with auth currentUser
      set objId "assignedTo" $ T.encodeUtf8 $ userLogin u

    ,\objId val -> maybe (return ()) ($objId)
      $ Map.lookup val actionResultMap
    ])
  ,("assignedTo",
    [\objId _val -> dateNow id >>= set objId "assignTime"
    ])
  ,("closed",
    [\objId -> \case
      "1" -> closeAction objId
      "0" -> do
        kazeId <- get objId "caseId"
        upd kazeId "actions" $ addToList objId
      _ -> error "action.closed not 0 or 1"
    ])
  ]

actionResultMap :: MonadTrigger m b => Map.Map ByteString (ObjectId -> m b ())
actionResultMap = Map.fromList
  [("busyLine",        \objId -> dateNow (+ (5*60))  >>= set objId "duetime" >> set objId "result" "")
  ,("callLater",       \objId -> dateNow (+ (30*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("partnerNotFound", \objId -> dateNow (+ (2*60*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("clientCanceledService", \objId -> closeAction objId >> sendSMS objId SmsTemplate.cancel >> sendMailToPSA objId)
  ,("unassignPlease",  \objId -> set objId "assignedTo" "" >> set objId "result" "")
  -- Defer an action by an amount of time specified in deferBy field
  -- in HH:MM format
  ,("defer",           \objId -> do
      deferBy <- get objId "deferBy"
      -- Deferring is a phantom result which is not preserved
      set objId "deferBy" "" >> set objId "result" ""  >> set objId "closeTime" ""
      -- Clear assignee when deferring actions
      clearAssignee objId
      case (map B.readInt $ B.split ':' deferBy) of
        (Just (hours, _):Just (minutes, _):_) ->
            when (0 <= hours && 0 <= minutes && minutes <= 59) $
                 dateNow (+ (60 * (hours * 60 + minutes)))
                             >>= set objId "duetime"
        _ -> return ()
  )
  ,("needPartner",     \objId -> do
     setServiceStatus objId "needPartner"
     newAction <- replaceAction
         "needPartner"
         "Требуется найти партнёра для оказания услуги"
         (identFv Role.bo_parguy) "1" (+60) objId
     clearAssignee newAction
  )
  ,("serviceOrdered", \objId -> do
    setServiceStatus objId "serviceOrdered"
    svcId    <- get objId "parentId"
    assignee <- get objId "assignedTo"
    set svcId "assignedTo" assignee

    act <- replaceAction
      "addBill"
      "Прикрепить счёт"
      (identFv Role.bo_bill) "1" (+14*24*60*60)
      objId
    clearAssignee act

    sendMailToPSA objId
    isReducedMode >>= \case
      True -> do
        closeServiceAndSendInfoVW objId
        sendMailToDealer objId
      False -> do
        act' <- replaceAction
          "tellClient"
          "Сообщить клиенту о договорённости"
          (identFv Role.bo_control) "1" (+60) objId
        Just u <- liftDb $ with auth currentUser
        tryToPassChainToControl u act'
  )
  ,("serviceOrderedSMS", \objId -> do
    sendSMS objId SmsTemplate.order

    setServiceStatus objId "serviceOrdered"
    svcId    <- get objId "parentId"
    assignee <- get objId "assignedTo"
    set svcId "assignedTo" assignee

    sendMailToPSA objId
    isReducedMode >>= \case
      True -> do
        closeServiceAndSendInfoVW objId
        sendMailToDealer objId
      False -> do
        tm <- getService objId "times_expectedServiceStart"
        act <- replaceAction
          "checkStatus"
          "Уточнить статус оказания услуги"
          (identFv Role.bo_control) "3" (changeTime (+5*60) tm)
          objId
        Just u <- liftDb $ with auth currentUser
        tryToPassChainToControl u act
  )
  ,("partnerNotOk", void .
    replaceAction
      "cancelService"
      "Требуется отказаться от заказанной услуги"
      (identFv Role.bo_control) "1" (+60)
  )
  ,("moveToAnalyst", \objId -> do
    act <- replaceAction
      "orderServiceAnalyst"
      "Заказ вторичной услуги"
      (identFv Role.bo_secondary) "1" (+60) objId
    clearAssignee act
  )
  ,("moveToBack", \objId -> do
    act <- replaceAction
      "orderService"
      "Заказ услуги оператором Back Office"
      (identFv Role.bo_order) "1" (+60) objId
    clearAssignee act
  )
  ,("needPartnerAnalyst",     \objId -> do
     setServiceStatus objId "needPartner"
     newAction <- replaceAction
         "needPartner"
         "Требуется найти партнёра для оказания услуги"
         (identFv Role.bo_parguy) "1" (+60) objId
     clearAssignee newAction
  )
  ,("serviceOrderedAnalyst", \objId -> do
    setServiceStatus objId "serviceOrdered"
    sendMailToPSA objId

    isReducedMode >>= \case
      True -> do
        closeAction objId
        sendMailToDealer objId
      False -> do
        act <- replaceAction
          "tellClient"
          "Сообщить клиенту о договорённости"
          (identFv Role.bo_control) "1" (+60) objId
        Just u <- liftDb $ with auth currentUser
        tryToPassChainToControl u act
  )
  ,("dealerNotApproved", void .
    replaceAction
      "tellDealerDenied"
      "Сообщить об отказе дилера"
      (identFv Role.bo_control) "3" (+60)
  )
  ,("carmakerNotApproved", void .
    replaceAction
      "tellMakerDenied"
      "Сообщить об отказе автопроизводителя"
      (identFv Role.bo_control) "3" (+60)
  )
  ,("partnerNotOkCancel", \objId -> do
      setServiceStatus objId "cancelService"
      void $ replaceAction
         "cancelService"
         "Требуется отказаться от заказанной услуги"
         (identFv Role.bo_control) "1" (+60) objId
  )
  ,("partnerOk", \objId ->
    isReducedMode >>= \case
      True -> closeAction objId
      False -> do
        tm <- getService objId "times_expectedServiceStart"
        void $ replaceAction
          "checkStatus"
          "Уточнить статус оказания услуги"
          (identFv Role.bo_control) "3" (changeTime (+5*60) tm)
          objId
  )
  ,("serviceDelayed", \objId -> do
    setServiceStatus objId "serviceDelayed"
    void $ replaceAction
      "tellDelayClient"
      "Сообщить клиенту о задержке начала оказания услуги"
      (identFv Role.bo_control) "1" (+60)
      objId
  )
  ,("serviceInProgress", \objId -> do
    setServiceStatus objId "serviceInProgress"
    isReducedMode >>= \case
      True -> closeAction objId
      False -> do
        tm <- getService objId "times_expectedServiceEnd"
        void $ replaceAction
          "checkEndOfService"
          "Уточнить у клиента окончено ли оказание услуги"
          (identFv Role.bo_control) "3" (changeTime (+5*60) tm)
          objId
  )
  ,("prescheduleService", \objId -> do
    setServiceStatus objId "serviceInProgress"
    isReducedMode >>= \case
      True -> closeAction objId
      False -> do
        void $ replaceAction
          "checkEndOfService"
          "Уточнить у клиента окончено ли оказание услуги"
          (identFv Role.bo_control) "3" (+60)
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
      (identFv Role.bo_control) "3" (changeTime (+5*60) tm)
      objId
  )
  ,("serviceFinished", \objId -> do
    closeServiceAndSendInfoVW objId
    sendSMS objId SmsTemplate.complete
    sendMailToDealer objId
  )
  ,("complaint", \objId -> do
    closeServiceAndSendInfoVW objId
    setService objId "clientSatisfied" "notSatis"
    act1 <- replaceAction
      "complaintResolution"
      "Клиент предъявил претензию"
      (identFv Role.bo_qa) "1" (+60)
      objId
    clearAssignee act1
  )
  ,("billNotReady", \objId -> dateNow (+ (5*24*60*60))  >>= set objId "duetime")
  ,("billAttached", \objId -> do
    act <- replaceAction
      "headCheck"
      "Проверка РКЦ"
      (identFv Role.head) "1" (+360) objId
    clearAssignee act
  )
  ,("parguyToBack", \objId -> do
    act <- replaceAction
      "parguyNeedInfo"
      "Менеджер по Партнёрам запросил доп. информацию"
      (identFv Role.bo_control) "3" (+360) objId
    clearAssignee act
  )
  ,("backToParyguy", \objId -> do
    act <- replaceAction
      "addBill"
      "Прикрепить счёт"
      (identFv Role.bo_bill) "1" (+360) objId
    clearAssignee act
  )
  ,("headToParyguy", \objId -> do
    act <- replaceAction
      "addBill"
      "На доработку МпП"
      (identFv Role.bo_bill) "1" (+360) objId
    clearAssignee act
  )
  ,("confirm", \objId -> do
    act <- replaceAction
      "directorCheck"
      "Проверка директором"
      (identFv Role.bo_director) "1" (+360) objId
    clearAssignee act
  )
  ,("confirmWODirector", \objId -> do
    act <- replaceAction
      "accountCheck"
      "Проверка бухгалтерией"
      (identFv Role.bo_account) "1" (+360) objId
    clearAssignee act
  )
  ,("confirmFinal", \objId -> do
    act <- replaceAction
      "analystCheck"
      "Обработка аналитиком"
      (identFv Role.bo_analyst) "1" (+360) objId
    clearAssignee act
  )
  ,("directorToHead", \objId -> do
    act <- replaceAction
      "headCheck"
      "Проверка РКЦ"
      (identFv Role.head) "1" (+360) objId
    clearAssignee act
  )
  ,("directorConfirm", \objId -> do
    act <- replaceAction
      "accountCheck"
      "Проверка бухгалтерией"
      (identFv Role.bo_account) "1" (+360) objId
    clearAssignee act
  )
  ,("dirConfirmFinal", \objId -> do
    act <- replaceAction
      "analystCheck"
      "Обработка аналитиком"
      (identFv Role.bo_analyst) "1" (+360) objId
    clearAssignee act
  )
  ,("vwclosed", \objId -> do
    sendMailToPSA objId
    closeAction objId
  )
  ,("complaintManaged", closeAction
  )
  ,("communicated", closeAction
  )
  ,("okButNoService", \objId -> do
    caseId <- get objId "caseId"
    get caseId "services" >>= \case
      "" -> set caseId "caseStatus" "s2" -- closed
      _  -> return ()
    closeAction objId
  )
  ,("accountConfirm", \objId -> do
    act <- replaceAction
      "analystCheck"
      "Обработка аналитиком"
      (identFv Role.bo_analyst) "1" (+360) objId
    clearAssignee act
  )
  ,("accountToDirector", \objId -> do
    act <- replaceAction
      "directorCheck"
      "Проверка директором"
      (identFv Role.bo_director) "1" (+360) objId
    clearAssignee act
  )
  ,("analystChecked", closeAction)
  ,("caseClosed", \objId -> do
    setServiceStatus objId "serviceClosed"
    closeAction objId
  )
  ,("partnerGivenCloseTime", \objId -> do
    tm <- getService objId "times_expectedServiceClosure"
    dateNow (changeTime (+5*60) tm) >>= set objId "duetime"
    set objId "result" "")
  ,("falseCallWBill", \objId -> do
     setService objId "falseCall" "bill"
     closeAction objId
     sendSMS objId SmsTemplate.cancel
  )
  ,("falseCallWOBill", \objId -> do
     setService objId "falseCall" "nobill"
     closeAction objId
     sendSMS objId SmsTemplate.cancel
  )
  ,("clientNotified", \objId -> do
     setServiceStatus objId "serviceClosed"
     closeAction objId
  )
  ,("notNeedService", \objId -> do
     setServiceStatus objId "serviceClosed"
     closeAction objId
  )
  ]

changeTime :: (Int -> Int) -> ByteString -> Int -> Int
changeTime fn x y = case B.readInt x of
  Just (r,"") -> fn r
  _ -> fn y

setService :: MonadTrigger m b => ObjectId -> FieldName -> FieldValue -> m b ()
setService objId field val = do
  svcId <- get objId "parentId"
  set svcId field val

-- Due to disabled trigger recursion we need to call
-- onRecursiveServiceStatusChange manually
-- on each service.status change
setServiceStatus :: MonadTrigger m b => ObjectId -> FieldName -> m b ()
setServiceStatus actId val = do
  svcId <- get actId "parentId"
  set svcId "status" val
  onRecursiveServiceStatusChange svcId val

getService :: MonadTrigger m b => ObjectId -> FieldName -> m b FieldValue
getService objId field
  = get objId "parentId"
  >>= (`get` field)

-- | Get the name of an action's service.
getServiceType :: MonadTrigger m b =>
                  ObjectId
               -- ^ Action id.
               -> m b (Maybe String)
getServiceType actId = do
  v <- get actId "parentId"
  return $ fst <$> read1Reference v


closeServiceAndSendInfoVW :: MonadTrigger m b => ObjectId -> m b ()
closeServiceAndSendInfoVW objId = do
  setServiceStatus objId "serviceOk"

  partner <- getService objId "contractor_partner"
  comment <- get objId "comment"
  let addParComment act = set act "comment"
        $ B.concat [utf8 "Партнёр: ", partner, "\n\n", comment]

  tm <- getService objId "times_expectedServiceClosure"
  act1 <- replaceAction
    "closeCase"
    "Закрыть заявку"
    (identFv Role.bo_close) "3" (changeTime (+7*24*60*60) tm)
    objId
  clearAssignee act1
  addParComment act1

  subprogram <- get objId "caseId" >>= (`get` "subprogram")
  st <- getServiceType objId
  when (subprogram `elem`
        (map identFv [ SubProgram.peugeot, SubProgram.citroen
                     , SubProgram.vwMotor, SubProgram.vwCargo])) $ do
    dueDelta <- if st == Just "tech"
                then do
                  fse <- getService objId "times_factServiceEnd"
                  return $ changeTime (+5*60) fse
                else
                  return (+7*24*60*60)
    act2 <- replaceAction
      "getInfoDealerVW"
      "Уточнить информацию о ремонте у дилера/партнёра (VW, PSA)"
      (identFv Role.bo_dealer) "3" dueDelta
      objId
    clearAssignee act2
    addParComment act2


closeAction :: MonadTrigger m b => ObjectId -> m b ()
closeAction objId = do
  kazeId <- get objId "caseId"
  upd kazeId "actions" $ dropFromList objId
  set objId "closed" "1"

replaceAction :: MonadTrigger m b =>
                 FieldValue
              -> String
              -> FieldValue
              -> FieldValue
              -> (Int -> Int)
              -> ObjectId
              -> m b ObjectId
replaceAction actionName actionDesc targetGroup priority dueDelta objId = do
  assignee <- get objId "assignedTo"
  svcId <- get objId "parentId"
  due <- dateNow dueDelta
  kazeId <- get svcId "parentId"
  now <- dateNow id
  actionId <- new "action" $ Map.fromList
    [("name", actionName)
    ,("ctime", now)
    ,("description", utf8 actionDesc)
    ,("targetGroup", targetGroup)
    ,("assignedTo", assignee)
    ,("assignTime", now)
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
requestFddsVin _ vin = do
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

srvCostCounted :: MonadTrigger m b => ObjectId -> m b FieldValue
srvCostCounted srvId = do
  falseCall        <- get srvId "falseCall"
  falseCallPercent <- get srvId "falseCallPercent" >>=
                      return . fromMaybe 100 . mbreadDouble
  tarifIds <- get srvId "cost_serviceTarifOptions" >>= return . B.split ','
  cost <- sum <$> mapM calcCost tarifIds
  case falseCall of
    "bill" -> return $ printBPrice $ cost * (falseCallPercent / 100)
    _      -> return $ printBPrice cost

calcCost :: MonadTrigger m b => ObjectId -> m b Double
calcCost objId = do
  p <- get objId "price" >>= return . fromMaybe 0 . mbreadDouble
  c <- get objId "count" >>= return . fromMaybe 0 . mbreadDouble
  return $ p * c

setSrvMCost :: MonadTrigger m b => ObjectId -> m b ()
setSrvMCost objId = do
  obj    <- readObject objId
  parent <- readObject $ fromJust $ Map.lookup "parentId" obj
  dict   <- liftDb $ gets rkcDict
  set objId "marginalCost" $ RKC.setSrvMCost srvName obj parent dict
    where
      -- readR   = lift . RC.read' redis
      srvName = head $ B.split ':' objId

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
