{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ModelTriggers
  (runCreateTriggers
  ,runUpdateTriggers
  ) where


import Prelude hiding (until)

import Control.Applicative
import Control.Monad
import Control.Monad.Free (Free)
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import Data.Time.Calendar
import Data.Time.Clock
import Data.Dynamic
import GHC.TypeLits

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple.Transaction as PG
import qualified Snap.Snaplet.PostgresqlSimple as PS

import WeatherApi (tempC)

import Application (AppHandler)
import Data.Model as Model
import Data.Model.Patch as Patch
import Data.Model.Types
import           Trigger.Dsl as Dsl

import           Carma.Model.Types (HMDiffTime(..), on, off)

import qualified Carma.Model.Action as Action
import qualified Carma.Model.ActionResult as ActionResult
import qualified Carma.Model.ActionType as ActionType
import qualified Carma.Model.BusinessRole as BusinessRole
import           Carma.Model.Call (Call)
import qualified Carma.Model.Call as Call
import           Carma.Model.Case (Case)
import qualified Carma.Model.Case as Case
import qualified Carma.Model.City as City
import qualified Carma.Model.CaseStatus as CS
import           Carma.Model.Contract as Contract hiding (ident)
import qualified Carma.Model.ContractCheckStatus as CCS
import           Carma.Model.Event (EventType(..))
import qualified Carma.Model.FalseCall as FC

import qualified Carma.Model.Service as Service
import           Carma.Model.Service (Service)
import qualified Carma.Model.Service.Hotel as Hotel
import qualified Carma.Model.Service.Rent as Rent
import qualified Carma.Model.Service.Taxi as Taxi
import qualified Carma.Model.Service.Tech as Tech
import qualified Carma.Model.Service.Towage as Towage
import           Carma.Model.SubProgram as SubProgram hiding (ident)

import qualified Carma.Model.ServiceStatus as SS
import qualified Carma.Model.TowType as TowType
import           Carma.Model.Usermeta (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta
import qualified Carma.Model.Diagnostics.Wazzup as Wazzup

import           Carma.Backoffice
import           Carma.Backoffice.DSL (ActionTypeI, Backoffice)
import qualified Carma.Backoffice.DSL as BO
import qualified Carma.Backoffice.Action.SMS as Action (sendSMS)
import           Carma.Backoffice.DSL.Types
import           Carma.Backoffice.Graph (startNode)

import           AppHandlers.ActionAssignment

-- TODO: rename
--   - trigOnModel -> onModel :: ModelCtr m c => c -> Free (Dsl m) res
--   - trigOnField -> onField


beforeCreate :: TriggerMap
beforeCreate = Map.unionsWith (++)
  [trigOnModel ([]::[Call])
    $ getCurrentUser >>= modifyPatch . Patch.put Call.callTaker
  ,trigOnModel ([]::[Usermeta]) $ do
    Just login <- getPatchField Usermeta.login -- TODO: check if valid?
    -- NB!
    -- Hope that Snap does not cache users and, in case of error during some
    -- further steps of Usermeta, this will be automatically rolled back.
    -- Otherwise we need some kind of finalisers for "Real world actions that
    -- could not be deferred".
    createSnapUser login

  , trigOnModel ([]::[Contract.Contract]) $ do
    getCurrentUser >>= modPut Contract.committer
    getNow >>= modPut Contract.ctime
    -- Set checkPeriod and validUntil from the subprogram. Remember to
    -- update vinnie_queue triggers when changing these!
    s <- getPatchField Contract.subprogram
    cp <- getPatchField Contract.checkPeriod
    case (s, cp) of
      (Just (Just s'), Nothing) -> do
        sp <- dbRead s'
        modPut Contract.checkPeriod (sp `get'` SubProgram.checkPeriod)
      _ -> return ()
    since <- getPatchField Contract.validSince
    until <- getPatchField Contract.validUntil
    case (s, since, until) of
      (Just (Just s'), Just (Just newSince), Nothing) ->
        fillValidUntil s' newSince
      _ -> return ()

  , trigOnModel ([]::[Case]) $ do
    n <- getNow
    getCurrentUser >>= modPut Case.callTaker
    modPut Case.callDate             $ Just n
    modPut Case.caseStatus             CS.front
    modPut Case.contact_contactOwner $ Just on
    getPatchField Case.comment >>=
      \case
        Just (Just wi) -> fillWazzup wi
        _              -> return ()

  , trigOnModel ([]::[Service]) $ do
    n <- getNow
    modPut Service.createTime         $ Just n
    modPut Service.times_expectedServiceStart $
      Just (addUTCTime (1 * BO.hours) n)
    modPut Service.times_factServiceStart $
      Just (addUTCTime (1 * BO.hours) n)
    modPut Service.times_expectedServiceEnd $
      Just (addUTCTime (2 * BO.hours) n)
    modPut Service.times_expectedServiceClosure $
      Just (addUTCTime (12 * BO.hours) n)
    modPut Service.times_factServiceClosure $
      Just (addUTCTime (12 * BO.hours) n)
    modPut Service.times_expectedDispatch $
      Just (addUTCTime (10 * BO.minutes) n)

    modPut Service.createTime         $ Just n
    modPut Service.falseCall            FC.none
    modPut Service.payment_overcosted $ Just off
    modPut Service.status               SS.creating
    modPut Service.urgentService      $ Just $ Ident "notUrgent"
    modPut Service.warrantyCase       $ Just off

  , trigOnModel ([]::[Hotel.Hotel]) $
    modPut Hotel.providedFor $ Just "0"

  , trigOnModel ([]::[Rent.Rent]) $
    modPut Rent.providedFor $ Just "0"

  , trigOnModel ([]::[Taxi.Taxi]) $ do
    p <- getPatch
    let parId = fromMaybe (error "No parent case") $
                p `Patch.get` parentField Service.parentId
    c <- dbRead parId
    modPut Taxi.taxiFrom_address $ c `Patch.get'` Case.caseAddress_address

  , trigOnModel ([]::[Tech.Tech]) $
    modPut Tech.suburbanMilage $ Just "0"

  , trigOnModel ([]::[Towage.Towage]) $ do
    modPut Towage.accident            $ Just off
    modPut Towage.canNeutral          $ Just off
    modPut Towage.manipulatorPossible $ Just off
    modPut Towage.suburbanMilage      $ Just "0"
    modPut Towage.towType             $ Just TowType.dealer
    modPut Towage.towerType           $ Just $ Ident "evac"
    modPut Towage.towingPointPresent  $ Just off
    modPut Towage.vandalism           $ Just off
    modPut Towage.wheelsUnblocked     $ Just $ Ident "w0"
  ]

afterCreate :: TriggerMap
afterCreate = Map.unionsWith (++)
  [trigOnModel ([]::[Usermeta]) updateSnapUserFromUsermeta
  ]

beforeUpdate :: TriggerMap
beforeUpdate = Map.unionsWith (++) $
  [trigOn Usermeta.businessRole $ \case
    Nothing -> return ()
    Just bRole -> do
      roles <- (`get'` BusinessRole.roles) <$> dbRead bRole
      modPut Usermeta.roles roles

  , trigOn ActionType.priority $
    \n -> modPut ActionType.priority $
          if
            | n < topPriority   -> topPriority
            | n > leastPriority -> leastPriority
            | otherwise         -> n

  , trigOn Action.result $ \case
      Nothing -> return ()
      Just _ -> do
        getNow >>= (modifyPatch . Patch.put Action.closeTime . Just)
        getCurrentUser >>= (modifyPatch . Patch.put Action.assignedTo . Just)

  , trigOn Action.assignedTo $ \case
      Nothing -> return ()
      Just _ -> getNow >>=
                (modifyPatch . Patch.put Action.assignTime . Just)

  -- Set validUntil from the subprogram. Remember to update
  -- vinnie_queue triggers when changing this!
  , trigOn Contract.validSince $ \case
       Nothing -> return ()
       Just newSince -> do
         cp <- getIdent >>= dbRead
         let oldSince = cp `get'` Contract.validSince
         do
           s  <- getPatchField Contract.subprogram
           vu <- getPatchField Contract.validUntil
           -- Prefer fields from the patch, but check DB as well
           let  nize (Just Nothing) = Nothing
                nize v              = v
                sub = (nize s) <|> (Just $ cp `get'` Contract.subprogram)
                -- We need to check if validUntil field is *missing*
                -- from both patch and DB, thus Just Nothing from DB
                -- is converted to Nothing
                until =
                  (nize vu) <|> (nize $ Just $ cp `get'` Contract.validUntil)
           case (sub, until) of
             (Just (Just s'), Nothing) ->
               when (oldSince /= Just newSince) $ fillValidUntil s' newSince
             _ -> return ()

  , trigOn Case.car_plateNum $ \case
      Nothing -> return ()
      Just val ->
        when (T.length val > 5) $
        modifyPatch (Patch.put Case.car_plateNum (Just $ T.toUpper val))

  , trigOn Case.comment $ \case
      Nothing -> return ()
      Just wi -> fillWazzup wi

  , trigOn Service.times_expectedServiceStart $ \case
      Nothing -> return ()
      Just tm ->
        modifyPatch $
        Patch.put Service.times_expectedServiceEnd
        (Just $ addUTCTime (1 * BO.hours) tm) .
        Patch.put Service.times_expectedServiceClosure
        (Just $ addUTCTime (11 * BO.hours) tm) .
        Patch.put Service.times_factServiceStart Nothing
  , trigOn Service.times_expectedDispatch $ const $
    modifyPatch (Patch.put Service.times_factServiceStart Nothing)
  , trigOn Service.times_expectedServiceEnd $ const $
    modifyPatch (Patch.put Service.times_factServiceEnd Nothing)
  , trigOn Service.times_expectedServiceClosure $ const $
    modifyPatch (Patch.put Service.times_factServiceClosure Nothing)

  , trigOn Case.city $ \case
      Nothing -> return ()
      Just city ->
        do
          cp <- dbRead city
          w <- getCityWeather (cp `get'` City.label)
          let temp = either (const $ Just "") (Just . T.pack . show . tempC) w
          modifyPatch (Patch.put Case.temperature temp)

  , trigOn Case.contract $ \case
      Nothing -> return ()
      Just cid ->
        do
          kase <- dbRead =<< getIdent
          contract <- dbRead cid
          n <- getNow
          let sinceExceeded =
                case contract `get'` Contract.validSince of
                  Just s  -> n < UTCTime (Contract.unWDay s) 0
                  Nothing -> False
              untilExceeded =
                case contract `get'` Contract.validUntil of
                  Just u  -> n > UTCTime (Contract.unWDay u) 0
                  Nothing -> False
              checkStatus = if sinceExceeded || untilExceeded
                            then CCS.vinExpired
                            else CCS.base
              p = map
                  (\(C2C conField f caseField) ->
                     let
                       new = f $ contract `Patch.get'` conField
                       old = kase `get'` caseField
                     in
                       case old of
                         Nothing -> Patch.put caseField new
                         Just sth -> if show sth == show ("" :: String)
                                     then Patch.put caseField new
                                     else id)
                  contractToCase
          modifyPatch $ foldl (flip (.)) id p
          modifyPatch (Patch.put Case.vinChecked $ Just checkStatus)
  ]  ++
  map entryToTrigger (fst carmaBackoffice) ++
  map actionToTrigger (snd carmaBackoffice)

afterUpdate :: TriggerMap
afterUpdate = Map.unionsWith (++) $
  [trigOn Usermeta.delayedState $ \_ -> wsMessage
  ,trigOn Usermeta.login        $ \_ -> updateSnapUserFromUsermeta
  ,trigOn Usermeta.password     $ \_ -> updateSnapUserFromUsermeta
  ,trigOn Usermeta.isActive     $ \_ -> updateSnapUserFromUsermeta
  ]

--  - runReadTriggers
--    - ephemeral fields
--      - moves logic from carma-models


-- Utility
----------------------------------------------------------------------

type ModelName = Text
type FieldName = Text
type TriggerMap = Map (ModelName, FieldName) [Dynamic]


-- | This is how we make new trigger
trigOn
  :: forall m name typ desc app res
  . (Model m, KnownSymbol name, Typeable typ)
  => (m -> Field typ (FOpt name desc app)) -- ^ watch this field
  -> (typ -> Free (Dsl m) res)             -- ^ run this if field changed
  -> TriggerMap
trigOn fld fun = Map.singleton (mName, fName) [toDyn fun']
  where
    mName = modelName (modelInfo :: ModelInfo m)
    fName = Model.fieldName fld
    fun'  = getPatchField fld >>= \case
      Nothing  -> error "BUG! We just triggered on this field. It MUST be there."
      Just val -> void $ fun val


trigOnModel
  :: forall m res . Model m
  => [m] -> Free (Dsl m) res -> TriggerMap
trigOnModel _ fun
  = Map.singleton
    (modelName (modelInfo :: ModelInfo m), "") -- dummy field name
    [toDyn $ void fun]


-- | This is how we run triggers on a patch

runCreateTriggers
  :: Model m
  => Patch m -> AppHandler (Either String (IdentI m, Patch m))
runCreateTriggers patch = do
  s <- PS.getPostgresState
  Pool.withResource (PS.pgPool s) $ \pg ->
    fmap (\st -> (st_ident st, st_patch st))
      <$> runTriggers beforeCreate afterCreate
        (getPatch >>= dbCreate >>= putIdentUnsafe)
        [""] -- pass dummy field name
        (emptyDslState undefined patch pg)


runUpdateTriggers
  :: Model m
  => IdentI m -> Patch m
  -> AppHandler (Either String (Patch m))
runUpdateTriggers ident patch = do
  s <- PS.getPostgresState
  Pool.withResource (PS.pgPool s) $ \pg ->
    fmap st_patch
      <$> runTriggers beforeUpdate afterUpdate
        (getPatch >>= dbUpdate ident >> return ())
        (HM.keys $ untypedPatch patch)
        (emptyDslState ident patch pg)


runTriggers
  :: forall m . Model m
  => TriggerMap -> TriggerMap -> DslM m () -> [FieldName]
  -> DslState m
  -> AppHandler (Either String (DslState m))
runTriggers before after dbAction fields state = do
  let mInfo = modelInfo :: ModelInfo m

  let matchingTriggers :: Model m' => Text -> TriggerMap -> [DslM m' ()]
      matchingTriggers model trigMap = do
        field <- fields
        Just triggers <- [Map.lookup (model,field) trigMap]
        trigger <- triggers
        return $ fromMaybe -- FIXME: fail early
          (fail $ printf "BUG! while casting tigger (%s,%s)"
            (show model) (show field))
          (fromDynamic trigger)

  let pg = st_pgcon state
  liftIO $ PG.beginLevel PG.ReadCommitted pg

  -- FIXME: try/finally
  res <- runDslM state $ do
    case parentInfo :: ParentInfo m of
      NoParent -> return ()
      ExParent p
        -> inParentContext
        $ sequence_ $ matchingTriggers (modelName p) before
    sequence_ $ matchingTriggers (modelName mInfo) before

    dbAction

    case parentInfo :: ParentInfo m of
      NoParent -> return ()
      ExParent p
        -> inParentContext
        $ sequence_ $ matchingTriggers (modelName p) after
    sequence_ $ matchingTriggers (modelName mInfo) after

  liftIO $ PG.commit pg

  case res of
    Left _   -> return ()
    Right st ->
      let fc = FutureContext $ st_pgcon st
      in liftIO $ sequence_ $ map ($ fc) $ st_futur st

  return res


-- | Mapping between a contract field and a  case field.
data Con2Case = forall t1 t2 n1 d1 n2 d2.
                (Eq t2, Show t2, FieldI t1 n1 d1, FieldI t2 n2 d2) =>
                C2C
                (Contract.Contract -> F (Maybe t1) n1 d1)
                (Maybe t1 -> Maybe t2)
                (Case.Case -> F (Maybe t2) n2 d2)


-- | Mapping between contract and case fields.
contractToCase :: [Con2Case]
contractToCase =
    [ C2C Contract.name id Case.contact_name
    , C2C Contract.vin id Case.car_vin
    , C2C Contract.make id Case.car_make
    , C2C Contract.model id Case.car_model
    , C2C Contract.seller id Case.car_seller
    , C2C Contract.plateNum id Case.car_plateNum
    , C2C Contract.makeYear id Case.car_makeYear
    , C2C Contract.color id Case.car_color
    , C2C Contract.buyDate (fmap Contract.unWDay) Case.car_buyDate
    , C2C Contract.lastCheckDealer id Case.car_dealerTO
    , C2C Contract.transmission id Case.car_transmission
    , C2C Contract.engineType id Case.car_engine
    , C2C Contract.engineVolume id Case.car_liters
    , C2C Contract.carClass id Case.car_class
    , C2C Contract.subprogram id Case.subprogram
    ]


-- | Set @validUntil@ field from a subprogram and a new @validSince@
-- value.
fillValidUntil :: IdentI SubProgram -> WDay -> Free (Dsl Contract) ()
fillValidUntil subprogram newSince = do
  sp <- dbRead subprogram
  let vf = sp `get'` SubProgram.validFor
      vs = unWDay newSince
  case vf of
    Just vf' ->
      modPut Contract.validUntil
      (Just WDay{unWDay = addDays (toInteger vf') vs})
    Nothing -> return ()


-- | Fill @diagnosisN@ fields.
fillWazzup :: IdentI Wazzup.Wazzup -> Free (Dsl Case) ()
fillWazzup wi = do
  wazz <- dbRead wi
  let f :: (FieldI t n d) => (Wazzup.Wazzup -> F t n d) -> t
      f = Patch.get' wazz
      p = Patch.put Case.diagnosis1 (f Wazzup.system) .
          Patch.put Case.diagnosis2 (f Wazzup.part) .
          Patch.put Case.diagnosis3 (f Wazzup.cause) .
          Patch.put Case.diagnosis4 (f Wazzup.suggestion)
  modifyPatch p


-- | Change a field in the patch.
modPut :: (KnownSymbol name, Typeable typ) =>
          (m -> Field typ (FOpt name desc app))
       -> typ
       -> Free (Dsl m) ()
modPut acc val = modifyPatch $ Patch.put acc val


haskellBinary :: (HaskellType t1 -> HaskellType t2 -> HaskellType t)
              -- ^ Non-lifted binary function.
              -> HaskellE t1
              -> HaskellE t2
              -> HaskellE t
haskellBinary fun a b = HaskellE $ fun <$> toHaskell a <*> toHaskell b


newtype HaskellE t = HaskellE { toHaskell :: Reader HCtx (HaskellType t) }
    deriving Typeable


instance Backoffice HaskellE where
    now = HaskellE $ asks ModelTriggers.now

    since nd t =
        HaskellE $ addUTCTime nd <$> toHaskell t

    nobody = nothing

    currentUser =
        HaskellE $
        Just <$> toHaskell (BO.userField Usermeta.ident)

    assigneeOfLast scope types res =
      HaskellE $ do
        res' <- mapM toHaskell res
        ids <- filteredActions scope types (res' :: [Maybe BO.ActionResultI])
        case ids of
          (l:_) -> return $ l `get'` Action.assignedTo
          []    -> return Nothing

    noResult = nothing

    previousAction =
        HaskellE $
        fromMaybe (Ident $ fst startNode) <$> asks prevAction

    userField acc =
        HaskellE $ asks (flip get' acc . user)

    serviceField acc =
        HaskellE $
        asks (flip get' acc . fromMaybe (error "No service") . service)

    caseField acc =
        HaskellE $
        asks (flip get' acc . kase)

    const = HaskellE . return

    just = HaskellE . return . Just

    req v =
      HaskellE $
      fromMaybe (error "Required value not set") <$> toHaskell v

    oneOf e lst =
      HaskellE $ flip elem lst <$> toHaskell e

    switch branches ow =
      HaskellE $
      case branches of
        ((c, br):bs) ->
          toHaskell c >>=
          \case
              True -> toHaskell br
              False -> toHaskell $ BO.switch bs ow
        [] -> toHaskell ow

    not a = HaskellE $ Prelude.not <$> toHaskell a

    (==) = haskellBinary (==)

    (>) = haskellBinary (Prelude.>)

    (&&) = haskellBinary (Prelude.&&)

    (||) = haskellBinary (Prelude.||)

    onField acc target body =
      mkTrigger acc target (`evalHaskell` body)

    insteadOf acc target body =
      mkTrigger acc target $
      \ctx -> do
        -- Reset to old value
        old <- dbRead =<< getIdent
        modifyPatch (Patch.put acc (old `get'` acc))
        evalHaskell ctx body

    setCaseField acc v =
        HaskellE $ do
          ctx <- ask
          let cid = kase ctx `Patch.get'` Case.ident
              val = evalHaskell ctx v
          return $ void $ dbUpdate cid $ put acc val Patch.empty

    setServiceField acc v =
        HaskellE $ do
          ctx <- ask
          sid <- srvId'
          return $ void $ setService sid acc (evalHaskell ctx v)

    sendMail _ = HaskellE $ return $ return ()

    sendSMS tpl = HaskellE $ inFuture . Action.sendSMS tpl <$> srvId'

    closePrevious scope types res =
      HaskellE $ do
        ctx <- ask
        targetActs <- filteredActions scope types [Nothing]
        return $ do
          let currentUser = user ctx `get'` Usermeta.ident
              -- Patch for closing actions
              p   =
                Patch.put Action.result (Just res) $
                -- Set current user as assignee if closing unassigned
                -- action
                --
                -- TODO this is identical to basic Action.result
                -- trigger, which we can't call programmatically
                Patch.put Action.assignedTo (Just currentUser) $
                Patch.put Action.closeTime (Just $ now ctx) Patch.empty
              -- Cause an action-closing event if we're canceling one
              -- of our own actions
              fakeClosing act =
                when (myAction && res == ActionResult.clientCanceledService) $
                void $ logCRUDState Update aid p
                  where
                    aid = act `get'` Action.ident
                    myAction = act `get'` Action.assignedTo == Just currentUser
          forM_ targetActs (\act ->
                              dbUpdate (act `get'` Action.ident) p >>
                              fakeClosing act)

    a *> b =
        HaskellE $ do
          ctx <- ask
          -- Freezing the context at the beginning of the chain we
          -- make all context-changing effects invisible to subsequent
          -- chain operators
          return $ evalHaskell ctx a >> evalHaskell ctx b

    proceed [] = HaskellE $ return $ return ()
    proceed (aT:ts) =
        HaskellE $ do
          ctx <- ask
          return $ do
            this <- getAction
            let -- Set current action as a source in the nested
                -- evaluator context
                ctx' = ctx{prevAction = (`get'` Action.aType) <$> this}
                (e, basePatch) = newActionData ctx' aT
                who = evalHaskell ctx' $ BO.assignment e

            -- Ignore insta-assignment for non-current users if
            -- target user is not Ready
            whoIfReady <-
              case who of
                Just u -> userIsReady u >>= \case
                  True -> return who
                  False ->
                    return $
                    if Just currentUser == who
                    then Just currentUser
                    else Nothing
                    where
                      currentUser = user ctx `get'` Usermeta.ident
                Nothing -> return Nothing

            let due = evalHaskell ctx' $ BO.due e
                -- Set assignTime + openTime if a user is picked
                ctime = now ctx
                nowIfWho = maybe Nothing (const $ Just ctime) whoIfReady
                p = Patch.put Action.duetime due $
                    Patch.put Action.openTime nowIfWho $
                    Patch.put Action.assignedTo whoIfReady $
                    Patch.put Action.assignTime nowIfWho $
                    Patch.put Action.parent ((`get'` Action.ident) <$> this)
                    basePatch
            dbCreate p >> evalHaskell ctx (BO.proceed ts)

    defer =
        HaskellE $ do
          ctx <- ask
          return $ do
            aid <- getIdent
            curPatch <- getPatch
            this <- dbRead aid
            let aT = this `get'` Action.aType
                -- Set current action as a source in the nested
                -- evaluator context
                ctx' = ctx{prevAction = Just aT}
                (_, basePatch) = newActionData ctx' aT

                dbDefer = fromMaybe (error "No deferBy in action") $
                          this `get'` Action.deferBy
                -- If there's no deferBy field in current patch, try
                -- to read it from DB for this action
                HMDiffTime deferBy =
                  case curPatch `get` Action.deferBy of
                    Just sth -> fromMaybe dbDefer sth
                    Nothing -> dbDefer

                -- Truncate everything below seconds, disregard leap
                -- seconds
                deferBy' = realToFrac deferBy
                due = addUTCTime deferBy' (now ctx)
                p = Patch.put Action.duetime due $
                    Patch.put Action.parent (Just aid)
                    basePatch
            void $ dbCreate p


-- | Trigger evaluator helper.
--
-- Upon entering the trigger, bootstrap available context for further
-- use in the trigger body. The trigger term itself does not use the
-- context.
mkTrigger :: (Eq (HaskellType t),
              FieldI (HaskellType t) n d, Model m,
              PreContextAccess m) =>
             (m -> F (HaskellType t) n d)
          -> HaskellE t
          -> (HCtx -> Free (Dsl m) ())
          -> HaskellE Trigger
mkTrigger acc target act =
  HaskellE $
  return $
  trigOn acc $
  \newVal -> do
    hctx <- mkContext Nothing
    when (newVal == evalHaskell hctx target) (act hctx)


-- | Produce a new context for nested Haskell evaluator call.
mkContext :: PreContextAccess m =>
             Maybe ActionTypeI
             -- ^ Previous action type.
          -> Free (Dsl m) HCtx
mkContext act = do
  srv <- getService
  kase' <- getKase
  usr <- dbRead =<< getCurrentUser
  acts <- caseActions $ kase' `get'` Case.ident
  t <- getNow
  return HCtx{ kase = kase'
             , service = srv
             , user = usr
             , actions = acts
             , prevAction = act
             , now = t
             }


-- | Graph entry and common data for new actions produced by 'proceed'
-- or 'defer'.
newActionData :: HCtx-> ActionTypeI -> (BO.Action, Patch Action.Action)
newActionData ctx aType = (e, p)
  where
    -- Never breaks for a valid back office
    e = fromMaybe (error "Current action unknown") $
        find ((== aType) . BO.aType) $
        snd carmaBackoffice
    p = Patch.put Action.ctime (now ctx) $
        Patch.put Action.targetGroup (evalHaskell ctx $ BO.targetRole e) $
        Patch.put Action.aType aType $
        Patch.put Action.caseId (kase ctx `get'` Case.ident) $
        Patch.put Action.serviceId ((`get'` Service.ident) <$> service ctx)
        Patch.empty


nothing :: HaskellE (Maybe t)
nothing = HaskellE $ return Nothing


-- | Obtain service id from the context or fail.
srvId' :: Reader HCtx (IdentI Service)
srvId' = do
  ctx <- ask
  return $
    fromMaybe (error "No service id in context") $
    (`get'` Service.ident) <$> service ctx


-- | Select some actions from the context.
filteredActions :: Scope
                -> [BO.ActionTypeI]
                -- ^ Matching action types.
                -> [Maybe BO.ActionResultI]
                -- ^ Matching action results.
                -> Reader HCtx [Object Action.Action]
filteredActions scope types resList = do
  ctx <- ask
  return $ do
    let sid = (`get'` Service.ident) <$> service ctx
    (`filter` actions ctx) $
      \act ->
        let
          typeOk   = (act `get'` Action.aType) `elem` types
          resultOk = (act `get'` Action.result) `elem` resList
          srvOk    = case scope of
                       InCase -> True
                       -- Filter actions by service if needed. Note that
                       -- *no* error is raised when called with InService
                       -- from service-less action effect
                       InService -> act `get'` Action.serviceId == sid
        in
          typeOk && resultOk && srvOk


evalHaskell :: HCtx -> HaskellE ty -> HaskellType ty
evalHaskell c t = runReader (toHaskell t) c


-- | Bootstrapping HaskellE context.
emptyContext :: HCtx
emptyContext = error "Empty context accessed (HaskellE interpreter bug)"


-- | Haskell evaluator context.
--
-- Enables data access via pure terms.
data HCtx =
    HCtx { kase       :: Object Case
         , user       :: Object Usermeta
         , service    :: Maybe (Object Service)
         , actions    :: [Object Action.Action]
         , prevAction :: Maybe ActionTypeI
         , now        :: UTCTime
         -- ^ Frozen time.
         }


entryToTrigger :: BO.Entry -> Map (ModelName, FieldName) [Dynamic]
entryToTrigger = evalHaskell emptyContext . BO.trigger


actionToTrigger :: BO.Action -> Map (ModelName, FieldName) [Dynamic]
actionToTrigger a =
  trigOn Action.result $
  \newVal -> do
    this <- dbRead =<< getIdent
    case newVal of
      Nothing -> return ()
      Just newRes -> do
        -- Skip changes for actions of different types
        when (this `get'` Action.aType == BO.aType a) $
          case lookup newRes (BO.outcomes a) of
            Just o -> do
              hctx <-
                case this `get'` Action.parent of
                  Nothing -> mkContext Nothing
                  Just pid -> do
                    parent <- dbRead pid
                    mkContext $ Just $ parent `get'` Action.aType
              evalHaskell hctx o
            Nothing -> error "Invalid action result"
