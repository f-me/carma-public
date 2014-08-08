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
import qualified Data.Model.Patch as Patch (Patch, get)
import qualified Data.Model.Patch.Sql as Patch (read)

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
import qualified Carma.Model.Diagnostics.Wazzup as Wazzup

import Util as U
import qualified Utils.Events  as Evt


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
    = add "towage" "contractor_address" [
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
        ,("case", Map.fromList
          [("caseStatus", [\kazeId st ->
            when (st == identFv CaseStatus.needInfo) $ do
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
           ])
          ,("comment", [\caseId val ->
            case fvIdent val of
              Nothing -> return ()
              Just wi -> do
                  res :: [Patch.Patch Wazzup.Wazzup] <-
                         liftDb $ withPG $ \c -> Patch.read wi c
                  case res of
                    [] -> return ()
                    (patch:_) ->
                        let
                            f acc = maybe "" identFv $
                                    fromMaybe Nothing $
                                    Patch.get patch acc
                            s = f Wazzup.system
                            p = f Wazzup.part
                            c = f Wazzup.cause
                            g = f Wazzup.suggestion
                        in
                          set caseId "diagnosis1" s >>
                          set caseId "diagnosis2" p >>
                          set caseId "diagnosis3" c >>
                          set caseId "diagnosis4" g >>
                          return ()
            ])
          ,("services", [\caseId _ -> updateCaseStatus caseId])
          -- ,("contact_name",
          --   [\objId val -> set objId "contact_name" $ upCaseStr val])
          -- ,("contact_ownerName",
          --   [\objId val -> set objId "contact_ownerName" $ upCaseStr val])
          ,("city", [setWeather])
          ,("car_plateNum", [\objId val ->
            when (T.length val > 5)
              $ set objId "car_plateNum" $ T.toUpper val])
          ,("contract", [\objId val ->
                         unless (T.null val) $
                           fillFromContract val objId >>= \case
                             Loaded -> set objId "vinChecked" $
                                       identFv CCS.base
                             Expired -> set objId "vinChecked" $
                                        identFv CCS.vinExpired
                             None -> return ()
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
    _        -> error $ "unknown BusinessRole id: " ++ show val

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
         (fromString $ unwords
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
      zipWithM_ (maybe (return ()) . setIfEmpty objId)
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
  :: MonadTrigger m b => ObjectId -> IdentI SS.ServiceStatus -> m b ()
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
        | identFv SS.creating `elem` statuses
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
  [("contractor_partnerId",
    [\objId val -> do
        srvs <- T.splitOn "," <$> get val "services"
        let m = head $ T.splitOn ":" objId
        s <- filterM (\s -> (m ==) <$> get s "serviceName") srvs
        case s of
          []     -> set objId "falseCallPercent" ""
          (x:_) -> get x "falseCallPercent" >>= set objId "falseCallPercent"
    ])
  ,("times_expectedServiceStart",
    [\objId val ->
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
      liftDb $ Evt.logLegacyCRUD Update objId Act.result
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
       liftDb $ Evt.logLegacyCRUD Update objId Act.openTime
     ])
   ]

actionResultMap :: MonadTrigger m b
                => Map.Map Text (ObjectId -> m b ())
actionResultMap = Map.empty


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
                    ObjectId -> IdentI SS.ServiceStatus -> m b ()
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
