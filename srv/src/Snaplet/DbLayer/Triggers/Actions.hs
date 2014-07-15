{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Snaplet.DbLayer.Triggers.Actions where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Maybe
import Data.String (fromString)

------------------------------------------------------------------------------
import WeatherApi (getWeather', tempC)
-----------------------------------------------------------------------------

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

import Carma.HTTP (read1Reference)


import           Data.Model

import qualified Carma.Model.Case as Case
import qualified Carma.Model.CaseStatus as CaseStatus
import qualified Carma.Model.Contract as Contract
import qualified Carma.Model.ContractCheckStatus as CCS
import qualified Carma.Model.PaymentType as PaymentType
import qualified Carma.Model.Program as Program
import qualified Carma.Model.SubProgram as SubProgram
import qualified Carma.Model.Role as Role
import qualified Carma.Model.ServiceStatus as SS
import qualified Carma.Model.SmsTemplate as SmsTemplate
import           Carma.Model.Event (EventType(..))
import qualified Carma.Model.Usermeta as Usermeta
import qualified Carma.Model.Action as Act
import qualified Carma.Model.Call   as Call

import Util as U
import qualified Utils.Events  as Evt
import qualified Utils.RKCCalc as RKC


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
          [("caseStatus", [\kazeId st ->
            if st == identFv CaseStatus.needInfo
            then do
              now <- dateNow Prelude.id
              due <- dateNow (+ (1*60))
              actionId <- new "action" $ Map.fromList
                [("name", "tellMeMore")
                ,("ctime", now)
                ,("duetime", due)
                ,("description", "Требуется дополнительная обработка кейса")
                ,("targetGroup", identFv Role.bo_order)
                ,("priority", "1")
                ,("caseId", kazeId)
                ,("closed", "0")
                ]
              upd kazeId "actions" $ addToList actionId
            else return ()])
          ,("services", [\caseId _ -> updateCaseStatus caseId])
          ,("partner", [\objId _ -> do
            mapM_ setSrvMCost =<< T.splitOn "," <$> get objId "services"
            ])
          ,("program", [\objId _ -> do
            mapM_ setSrvMCost =<< T.splitOn "," <$> get objId "services"
            ])
          -- ,("contact_name",
          --   [\objId val -> set objId "contact_name" $ upCaseStr val])
          -- ,("contact_ownerName",
          --   [\objId val -> set objId "contact_ownerName" $ upCaseStr val])
          ,("city", [setWeather])
          ,("car_plateNum", [\objId val ->
            when (T.length val > 5)
              $ set objId "car_plateNum" $ T.toUpper val])
          ,("contract", [\objId val ->
                         if (not $ T.null val)
                         then do
                           fillFromContract val objId >>= \case
                             Loaded -> set objId "vinChecked" $
                                       identFv CCS.base
                             Expired -> set objId "vinChecked" $
                                        identFv CCS.vinExpired
                             None -> return ()
                         else return ()
                        ])
          ,("psaExportNeeded",
            [\caseRef val -> when (val == "1") $ tryRepTowageMail caseRef])
          ])
        ,("call", Map.fromList
          [("endDate", [\objId _ ->
             liftDb $ Evt.logLegacyCRUD Update objId Call.endDate])
          ])
        ,("usermeta", Map.fromList
          [("delayedState", [\objId _ ->
             liftDb $ Evt.logLegacyCRUD Update objId Usermeta.delayedState])
          ,("businessRole", [updateBusinessRole])
          ])
        ]

updateBusinessRole :: MonadTrigger m b => ObjectId -> FieldValue -> m b ()
updateBusinessRole _     ""  = return ()
updateBusinessRole objId val =  do
  rs <- liftDb $ PG.query (fromString $
    "select array_to_string(roles, ',') from \"BusinessRole\"" ++
    " where id :: text = ?;") (Only val)
  case rs of
    [Only r] -> set objId "roles" r
    _        -> error $ "unknown BusinessRole id: " ++ (show val)

-- | Mapping between contract and case fields.
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


data ContractFillResult = None
                        -- ^ No contract found.
                        | Loaded
                        -- ^ Contract loaded from database.
                        | Expired
                        -- ^ Contract loaded and is expired.


fillFromContract :: MonadTrigger m b =>
                    ObjectId
                 -- ^ Contract id.
                 -> ObjectId
                 -> m b ContractFillResult
fillFromContract contract objId = do
  let cid :: IdentI Contract.Contract
      cid = case T.decimal contract of
          Right (i,_) -> Ident i
          _           -> error "Could not read contract id"
      contractTable = PT $ tableName (modelInfo :: ModelInfo Contract.Contract)
      programTable = PT $ tableName $
                     (modelInfo :: ModelInfo Program.Program)
      subProgramTable = PT $ tableName $
                        (modelInfo :: ModelInfo SubProgram.SubProgram)
  res <- liftDb $ PG.query
         (fromString $ intercalate " "
          [ "SELECT"
          -- 2 * M arguments, where M is the length of contractToCase
          , intercalate "," $ map (const "?.?::text") contractToCase
          -- 1: program id field
          , ", p.?::text"
            -- 1 argument: Contract table name
          , "FROM \"?\" c"
          -- 3 more parameters: SubProgram table name, Contract
          -- subprogram field, subprogram id field.
          , "JOIN \"?\" s ON c.? = s.?"
          -- 3 more parameters: Program table name, SubProgram parent
          -- field, program id field.
          , "JOIN \"?\" p ON s.? = p.?"
            -- 2 more arguments: contract id field, contract id value
          , "WHERE c.? = ?;"
          ]) $
         -- Selected fields
         ToRowList
         (map (\f -> (PT "c", PT $ fieldNameE $ fst f)) contractToCase)
         -- 2
         :. (Only $ PT $ fieldName Program.ident)
         :. (Only contractTable)
         -- 3
         :. (Only subProgramTable)
         :. (PT $ fieldName Contract.subprogram,
             PT $ fieldName SubProgram.ident)
         -- 3
         :. Only programTable
         :. (PT $ fieldName SubProgram.parent, PT $ fieldName Program.ident)
         -- 2
         :. (PT $ fieldName Contract.ident, cid)
  case res of
    [] -> return None
    [row] -> do
      -- Replace only empty fields of case
      let setIfEmpty oid nm val = get oid nm >>= \case
              "" -> set objId nm val
              _  -> return ()
      zipWithM_ (maybe (return ()) . (setIfEmpty objId))
                (map (fieldNameE . snd) $
                 contractToCase ++ [(undefined, FA Case.program)])
                row
      resExp <- liftDb $ PG.query
                [sql|SELECT ((now() < ?) or (? < now())) FROM "?" WHERE ? = ?;|]
                ( PT $ fieldName Contract.validSince
                , PT $ fieldName Contract.validUntil
                , contractTable
                , PT $ fieldName Contract.ident
                , cid)
      return $ case resExp of
                 [Only (Just True)] -> Expired
                 _                  -> Loaded
    _ -> error "fillFromContract: Contract primary key is broken"


-- | This is called when service status is changed in some trigger.
onRecursiveServiceStatusChange
  :: MonadTrigger m b => ObjectId -> (IdentI SS.ServiceStatus) -> m b ()
onRecursiveServiceStatusChange svcId val = do
  caseId  <- get svcId "parentId"

  -- Check if we need to send message to Genser
  payType <- get svcId "payType"
  pgm     <- get caseId "program"
  let (svc:_) = T.splitOn ":" svcId
  when (svc == "towage"
      && pgm == identFv Program.genser
      && payType == identFv PaymentType.ruamc
      && val `elem` [ SS.ordered
                    , SS.ok
                    , SS.canceled
                    , SS.clientCanceled])
    $ sendMailToGenser svcId

  updateCaseStatus caseId


-- | Automatically change case status according to statuses
-- of the contained services.
updateCaseStatus :: MonadTrigger m b => ObjectId -> m b ()
updateCaseStatus caseId =
  set caseId "caseStatus" =<< do
    servs <- T.splitOn "," <$> get caseId "services"
    statuses <- mapM (`get` "status") servs
    return $ identFv $ case statuses of
      _ | all (`elem`
               (map identFv [ SS.closed
                            , SS.falseCall
                            , SS.mistake])) statuses
          -> CaseStatus.closed
        | all (`elem`
               (map identFv [ SS.clientCanceled
                            , SS.closed])) statuses
          -> CaseStatus.closed
        | all (`elem`
               (map identFv [ SS.clientCanceled
                            , SS.canceled])) statuses
          -> CaseStatus.canceled
        | any (== (identFv SS.creating)) statuses
          -> CaseStatus.front
        | otherwise -> CaseStatus.back


-- | Clear assignee of control-class action chain head (which has
-- `bo_control` in `targetGroup`) unless the user has both
-- `bo_control` and `bo_order` roles. This will enable the action to
-- be pulled from action pool by bo_control users.
tryToPassChainToControl :: MonadTrigger m b =>
                           AuthUser -> ObjectId -> m b ()
tryToPassChainToControl user action = do
  let tr = Role . T.encodeUtf8 . identFv
  when
    (not (elem (tr Role.bo_control) (userRoles user)
      &&  elem (tr Role.bo_order) (userRoles user)))
    $ clearAssignee action


-- | Clear assignee and assignTime of an action.
clearAssignee :: MonadTrigger m b => ObjectId -> m b ()
clearAssignee action = set action "assignedTo" "" >> set action "assignTime" ""

serviceActions :: MonadTrigger m b
               => Map.Map FieldName [ObjectId -> FieldValue -> m b ()]
serviceActions = Map.fromList
  [("status", [\objId val ->
    let
     proceed s
      | s == Just SS.backoffice = do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          -- Check if backoffice transfer is related to callMeMaybe action
          relatedUser <- liftDb $ PG.query [sql|
            SELECT coalesce(a.assignedTo, '') FROM actiontbl a, usermetatbl u
              WHERE a.caseId = ?
                AND u.login = a.assignedTo
                AND (lastlogout IS NULL OR lastlogout < lastactivity)
                AND now() - lastactivity < '30 min'
                AND a.name IN ('tellMeMore', 'callMeMaybe')
                AND coalesce(a.result, '') <> 'communicated'
              LIMIT 1
            |] [kazeId]
          now <- dateNow id
          let (assignee, assignTime) =
                  case relatedUser of
                    [[u]] -> (u, now)
                    _     -> ("", "")
          actionId <- new "action" $ Map.fromList
            [("name", "orderService")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", "Заказать услугу")
            ,("targetGroup", identFv Role.bo_order)
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ,("assignTime", assignTime)
            ,("assignedTo", assignee)
            ]
          upd kazeId "actions" $ addToList actionId
          future $ sendSMS actionId SmsTemplate.create
      | s == Just SS.recallClient = do
          now <- dateNow id
          due <- dateNow (+ (15*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "tellMeMore")
            ,("ctime", now)
            ,("duetime", due)
            ,("description",  "Заказ услуги (требуется дополнительная информация)")
            ,("targetGroup", identFv Role.bo_order)
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
          future $ sendSMS actionId SmsTemplate.create
      | s == Just SS.ordered = do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          Just u <- liftDb $ with auth currentUser
          now <- dateNow id
          act1 <- new "action" $ Map.fromList
            [("name", "tellClient")
            ,("ctime", now)
            ,("duetime", due)
            ,("assignTime", now)
            ,("description", "Сообщить клиенту о договорённости")
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
            ,("description", "Прикрепить счёт")
            ,("targetGroup", identFv Role.bo_bill)
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("assignedTo", "")
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList act2
      | s == Just SS.mechanicConf = do
          now <- dateNow id
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "mechanicConf")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", "Требуется конференция с механиком")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
          future $ sendSMS actionId SmsTemplate.create
      | s == Just SS.dealerConf = do
          now <- dateNow id
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "dealerConf")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", "Требуется конференция с дилером")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
          future $ sendSMS actionId SmsTemplate.create
      | s == Just SS.checkNeeded = do
          now <- dateNow id
          due <- dateNow (+ (5*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "checkStatus")
            ,("ctime", now)
            ,("duetime", due)
            ,("description",
                "Клиент попросил уточнить, когда начнётся оказание услуги")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "3")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      | s == Just SS.dealerConfirmation = do
          now <- dateNow id
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "dealerApproval")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", "Требуется согласование с дилером")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      | s == Just SS.makerConfirmation = do
          now <- dateNow id
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "carmakerApproval")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", "Требуется согласование с заказчиком программы")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      | s == Just SS.clientCanceled = do
          now <- dateNow id
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "cancelService")
            ,("ctime", now)
            ,("duetime", due)
            ,("description", "Клиент отказался от услуги (сообщил об этом оператору Front Office)")
            ,("targetGroup", identFv Role.bo_control)
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      | otherwise = return ()
    in
      proceed (fvIdent val)
    -- Another one service status trigger.
    -- Sets corresponding case status.
    ,\objId val -> do
      set objId "status" val -- push change to the commit stack
      onRecursiveServiceStatusChange objId $
        fromMaybe (error "Unknown service status") (fvIdent val)
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
              ,("description", "Клиент предъявил претензию")
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
        let ids = T.splitOn "," opts
        redisDel ids >> set objId "cost_serviceTarifOptions" ""
    ])
  ,("falseCall",
    [\objId _ -> set objId "cost_counted" =<< srvCostCounted objId])
  ,("contractor_partnerId",
    [\objId val -> do
        srvs <- T.splitOn "," <$> get val "services"
        let m = head $ T.splitOn ":" objId
        s <- filterM (\s -> (m ==) <$> get s "serviceName") srvs
        case s of
          []     -> set objId "falseCallPercent" ""
          (x:_) -> get x "falseCallPercent" >>= set objId "falseCallPercent"
    ])
  ,("payType",
    [\objId val -> do
        case selectPrice val of
          Nothing       -> set objId "cost_counted" ""
          Just priceSel -> do
            ids <- T.splitOn "," <$> get objId "cost_serviceTarifOptions"
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
      case T.decimal val of
        Right (tm, _) -> do
          let h = 3600 :: Int -- seconds
          set objId "times_expectedServiceEnd"     $ T.pack $ show $ tm + 1*h
          set objId "times_expectedServiceClosure" $ T.pack $ show $ tm + 11*h
          set objId "times_factServiceStart" ""
        _ -> return ()
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

actionActions :: (MonadTrigger m b)
              => Map.Map Text [ObjectId -> FieldValue -> m b ()]
actionActions = Map.fromList
  [("result",
    [\objId val -> when (val `elem` resultSet1) $ do
         setServiceStatus objId SS.order
         void $ replaceAction
             "orderService"
             "Заказать услугу"
             (identFv Role.bo_order) "1" (+5*60) objId

    ,\objId _al -> do
      dateNow id >>= set objId "closeTime"
      Just u <- liftDb $ with auth currentUser
      set objId "assignedTo" $ userLogin u

    ,\objId val -> maybe (return ()) ($objId)
      $ Map.lookup val actionResultMap
    ,\objId _ ->
      liftDb $ Evt.logLegacyCRUD Update (objId) Act.result
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
   ,("openTime",
     [\objId _ ->
       liftDb $ Evt.logLegacyCRUD Update (objId) Act.openTime
     ])
   ]

actionResultMap :: MonadTrigger m b
                => Map.Map Text (ObjectId -> m b ())
actionResultMap = Map.fromList
  [("busyLine",        \objId -> dateNow (+ (5*60))  >>= set objId "duetime" >> set objId "result" "")
  ,("callLater",       \objId -> dateNow (+ (30*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("partnerNotFound", \objId -> dateNow (+ (2*60*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("clientCanceledService", \objId -> do
      closeAction objId
      liftDb $ sendSMS objId SmsTemplate.cancel
      sendMailToPSA objId
   )
  ,("unassignPlease",  \objId -> set objId "assignedTo" "" >> set objId "result" "")
  -- Defer an action by an amount of time specified in deferBy field
  -- in HH:MM format
  ,("defer",           \objId -> do
      deferBy <- get objId "deferBy"
      -- Deferring is a phantom result which is not preserved
      set objId "deferBy" "" >> set objId "result" ""  >> set objId "closeTime" ""
      name <- get objId "name"
      -- Clear assignee when deferring order-class actions
      when (name `elem` [ "orderService"
                        , "callMeMaybe"
                        , "tellMeMore"
                        , "orderServiceAnalyst"]) $
           clearAssignee objId
      case (map T.decimal $ T.splitOn ":" deferBy) of
        (Right (hours, _):Right (minutes, _):_) ->
            when (0 <= hours && 0 <= minutes && minutes <= 59) $
                 dateNow (+ (60 * (hours * 60 + minutes)))
                             >>= set objId "duetime"
        _ -> return ()
  )
  ,("needPartner",     \objId -> do
     setServiceStatus objId SS.needPartner
     newAction <- replaceAction
         "needPartner"
         "Требуется найти партнёра для оказания услуги"
         (identFv Role.bo_parguy) "1" (+60) objId
     clearAssignee newAction
  )
  ,("serviceOrdered", \objId -> do
    setServiceStatus objId SS.ordered
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
    act' <- replaceAction
      "tellClient"
      "Сообщить клиенту о договорённости"
      (identFv Role.bo_control) "1" (+60) objId
    Just u <- liftDb $ with auth currentUser
    tryToPassChainToControl u act'
  )
  ,("serviceOrderedSMS", \objId -> do
    liftDb $ sendSMS objId SmsTemplate.order

    setServiceStatus objId SS.ordered
    svcId    <- get objId "parentId"
    assignee <- get objId "assignedTo"
    set svcId "assignedTo" assignee

    sendMailToPSA objId
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
     setServiceStatus objId SS.needPartner
     newAction <- replaceAction
         "needPartner"
         "Требуется найти партнёра для оказания услуги"
         (identFv Role.bo_parguy) "1" (+60) objId
     clearAssignee newAction
  )
  ,("serviceOrderedAnalyst", \objId -> do
    setServiceStatus objId SS.ordered
    sendMailToPSA objId

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
      setServiceStatus objId SS.canceled
      void $ replaceAction
         "cancelService"
         "Требуется отказаться от заказанной услуги"
         (identFv Role.bo_control) "1" (+60) objId
  )
  ,("partnerOk", \objId -> do
        tm <- getService objId "times_expectedServiceStart"
        void $ replaceAction
          "checkStatus"
          "Уточнить статус оказания услуги"
          (identFv Role.bo_control) "3" (changeTime (+5*60) tm)
          objId
  )
  ,("serviceDelayed", \objId -> do
    setServiceStatus objId SS.delayed
    void $ replaceAction
      "tellDelayClient"
      "Сообщить клиенту о задержке начала оказания услуги"
      (identFv Role.bo_control) "1" (+60)
      objId
  )
  ,("serviceInProgress", \objId -> do
    setServiceStatus objId SS.inProgress
    tm <- getService objId "times_expectedServiceEnd"
    void $ replaceAction
      "checkEndOfService"
      "Уточнить у клиента окончено ли оказание услуги"
      (identFv Role.bo_control) "3" (changeTime (+5*60) tm)
      objId
  )
  ,("prescheduleService", \objId -> do
    setServiceStatus objId SS.inProgress
    void $ replaceAction
      "checkEndOfService"
      "Уточнить у клиента окончено ли оказание услуги"
      (identFv Role.bo_control) "3" (+60)
      objId
  )
  ,("serviceStillInProgress", \objId -> do
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
    liftDb $ sendSMS objId SmsTemplate.complete
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
      "" -> set caseId "caseStatus" (identFv CaseStatus.closed)
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
    setServiceStatus objId SS.closed
    closeAction objId
  )
  ,("partnerGivenCloseTime", \objId -> do
    tm <- getService objId "times_expectedServiceClosure"
    dateNow (changeTime (+5*60) tm) >>= set objId "duetime"
    set objId "result" "")
  ,("falseCallWBill", \objId -> do
     setService objId "falseCall" "bill"
     closeAction objId
     liftDb $ sendSMS objId SmsTemplate.cancel
  )
  ,("falseCallWOBill", \objId -> do
     setService objId "falseCall" "nobill"
     closeAction objId
     liftDb $ sendSMS objId SmsTemplate.cancel
  )
  ,("clientNotified", \objId -> do
     setServiceStatus objId SS.closed
     closeAction objId
  )
  ,("notNeedService", \objId -> do
     setServiceStatus objId SS.closed
     closeAction objId
  )
  ]

changeTime :: (Int -> Int) -> Text -> Int -> Int
changeTime fn x y = case T.decimal x of
  Right (r,"") -> fn r
  _ -> fn y

setService :: MonadTrigger m b => ObjectId -> FieldName -> FieldValue -> m b ()
setService objId field val = do
  svcId <- get objId "parentId"
  set svcId field val

-- Due to disabled trigger recursion we need to call
-- onRecursiveServiceStatusChange manually
-- on each service.status change
setServiceStatus :: MonadTrigger m b =>
                    ObjectId -> (IdentI SS.ServiceStatus) -> m b ()
setServiceStatus actId val = do
  svcId <- get actId "parentId"
  set svcId "status" (identFv val)
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
  v <- T.encodeUtf8 <$> get actId "parentId"
  return $ fst <$> read1Reference v


closeServiceAndSendInfoVW :: MonadTrigger m b => ObjectId -> m b ()
closeServiceAndSendInfoVW objId = do
  setServiceStatus objId SS.ok

  partner <- getService objId "contractor_partner"
  comment <- get objId "comment"
  let addParComment act = set act "comment"
        $ T.concat ["Партнёр: ", partner, "\n\n", comment]

  tm <- getService objId "times_expectedServiceClosure"
  act1 <- replaceAction
    "closeCase"
    "Закрыть заявку"
    (identFv Role.bo_close) "3" (changeTime (+7*24*60*60) tm)
    objId
  clearAssignee act1
  addParComment act1

  program <- get objId "caseId" >>= (`get` "program")
  st <- getServiceType objId
  when (program `elem`
        (map identFv [Program.vw, Program.peugeot, Program.citroen])) $ do
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
              -> Text
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
    ,("description", actionDesc)
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

setWeather :: MonadTrigger m b => ObjectId -> Text -> m b ()
setWeather objId city = do
  conf    <- liftDb $ gets weather
  weather <- liftIO $ getWeather' conf $ T.unpack $ T.filter (/= '\'') city
  case weather of
    Right w   -> do
      syslogJSON Debug "trigger/weather"
        [ "objId" .= objId
        , "city"  .=  city
        , "res"   .= show w
        ]
      set objId "temperature" $ T.pack $ show $ tempC w
    Left  err -> do
      set objId "temperature" ""
      syslogJSON Debug "trigger/weather"
        [ "objId" .= objId
        , "city"  .= city
        , "error" .= show err
        ]

srvCostCounted :: MonadTrigger m b => ObjectId -> m b FieldValue
srvCostCounted srvId = do
  falseCall        <- get srvId "falseCall"
  falseCallPercent <- get srvId "falseCallPercent" >>=
                      return . fromMaybe 100 . mbreadDouble
  tarifIds <- T.splitOn "," <$> get srvId "cost_serviceTarifOptions"
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
      srvName = head $ T.splitOn ":" objId
