{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ModelTriggers
  (runCreateTriggers
  ,runUpdateTriggers
  ) where


import Control.Applicative
import Control.Monad
import Control.Monad.Free (Free)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Dynamic
import GHC.TypeLits

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple.Transaction as PG
import qualified Snap.Snaplet.PostgresqlSimple as PS

import Application (AppHandler)
import Data.Model as Model
import Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch
import Data.Model.Types


import           Trigger.Dsl

import           Carma.Model.Types (HMDiffTime(..))

import qualified Carma.Model.Action as Action
import qualified Carma.Model.BusinessRole as BusinessRole
import           Carma.Model.Call (Call)
import qualified Carma.Model.Call as Call
import           Carma.Model.Case (Case)
import qualified Carma.Model.Case as Case
import qualified Carma.Model.Service as Service
import           Carma.Model.Service (Service)
import           Carma.Model.Usermeta (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta
import qualified Carma.Model.Diagnostics.Wazzup as Wazzup

import           Carma.Backoffice
import           Carma.Backoffice.DSL (ActionTypeI, Backoffice)
import qualified Carma.Backoffice.DSL as BO
import           Carma.Backoffice.DSL.Types
import           Carma.Backoffice.Graph (startNode)


-- TODO: rename
--   - trigOnModel -> onModel :: ModelCtr m c => c -> Free (Dsl m) res
--   - trigOnField -> onField


beforeCreate :: Map ModelName [Dynamic]
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
  ]


afterCreate :: Map ModelName [Dynamic]
afterCreate = Map.unionsWith (++)
  [trigOnModel ([]::[Usermeta]) updateSnapUserFromUsermeta
  ]

beforeUpdate :: Map (ModelName, FieldName) [Dynamic]
beforeUpdate = Map.unionsWith (++)
  [trigOn Usermeta.businessRole $ \case
    Nothing -> return ()
    Just bRole -> do
      Just roles <- (`Patch.get` BusinessRole.roles) <$> dbRead bRole
      modifyPatch $ Patch.put Usermeta.roles roles
  , trigOn Action.result $ \case
      Nothing -> return ()
      Just _ -> getNow >>=
                (modifyPatch . Patch.put Action.closeTime . Just)
  , trigOn Action.assignedTo $ \case
      Nothing -> return ()
      Just _ -> getNow >>=
                (modifyPatch . Patch.put Action.assignTime . Just)
  , trigOn Case.car_plateNum $ \case
      Nothing -> return ()
      Just val ->
        when (T.length val > 5) $
        modifyPatch (Patch.put Case.car_plateNum (Just $ T.toUpper val))
  , trigOn Case.comment $ \case
      Nothing -> return ()
      Just wi -> do
        wazz <- dbRead wi
        let f :: (FieldI t n d) => (Wazzup.Wazzup -> F t n d) -> t
            f = Patch.get' wazz
            p = Patch.put Case.diagnosis1 (f Wazzup.system) .
                Patch.put Case.diagnosis2 (f Wazzup.part) .
                Patch.put Case.diagnosis3 (f Wazzup.cause) .
                Patch.put Case.diagnosis4 (f Wazzup.suggestion)
        modifyPatch p
  , trigOn Service.times_expectedServiceStart $ \case
      Nothing -> return ()
      Just tm ->
        modifyPatch $
        (Patch.put Service.times_expectedServiceEnd
         (Just $ addUTCTime (1 * BO.hours) tm)) .
        (Patch.put Service.times_expectedServiceClosure
         (Just $ addUTCTime (11 * BO.hours) tm)) .
        (Patch.put Service.times_factServiceStart Nothing)
  , trigOn Service.times_expectedDispatch $ const $
    modifyPatch (Patch.put Service.times_factServiceStart Nothing)
  , trigOn Service.times_expectedServiceEnd $ const $
    modifyPatch (Patch.put Service.times_factServiceEnd Nothing)
  , trigOn Service.times_expectedServiceClosure $ const $
    modifyPatch (Patch.put Service.times_factServiceClosure Nothing)
  ]

afterUpdate :: Map (ModelName, FieldName) [Dynamic]
afterUpdate = Map.unionsWith (++) $
  [trigOn Usermeta.delayedState $ \_ -> wsMessage
  ,trigOn Usermeta.login        $ \_ -> updateSnapUserFromUsermeta
  ,trigOn Usermeta.password     $ \_ -> updateSnapUserFromUsermeta
  ,trigOn Usermeta.isActive     $ \_ -> updateSnapUserFromUsermeta
  ] ++
  map (evalHaskell emptyContext . BO.trigger) (fst carmaBackoffice)

--  - runReadTriggers
--    - ephemeral fields
--      - moves logic from carma-models


-- Utility
----------------------------------------------------------------------

type ModelName = Text
type FieldName = Text


-- | This is how we make new trigger
trigOn
  :: forall m name typ desc app res
  . (Model m, KnownSymbol name, Typeable typ)
  => (m -> Field typ (FOpt name desc app)) -- ^ watch this field
  -> (typ -> Free (Dsl m) res)             -- ^ run this if field changed
  -> Map (ModelName, FieldName) [Dynamic]
trigOn fld fun = Map.singleton (mName, fName) [toDyn fun']
  where
    mName = modelName (modelInfo :: ModelInfo m)
    fName = Model.fieldName fld
    fun'  = getPatchField fld >>= \case
      Nothing  -> error "BUG! We just triggered on this field. It MUST be there."
      Just val -> fun val >> tOk


trigOnModel
  :: forall m res . Model m
  => [m] -> Free (Dsl m) res -> Map ModelName [Dynamic]
trigOnModel _ fun
  = Map.singleton
    (modelName (modelInfo :: ModelInfo m))
    [toDyn $ fun >> tOk]


-- | This is how we run triggers on a patch

runCreateTriggers
  :: forall m . Model m
  => Patch m -> AppHandler (Either String (IdentI m, Patch m))
runCreateTriggers patch = do
  let mName = modelName (modelInfo :: ModelInfo m)
      pName = parentName (modelInfo :: ModelInfo m)

      matchingTriggers m
        = let ts = Map.findWithDefault [] mName m
          in maybe [] (\p -> Map.findWithDefault [] p m) pName ++ ts

      joinTriggers k tr = do
        let tr' = fromDyn tr $ tError 500 "dynamic BUG"
        tr' >>= either (return . Left) (const k)

      before = foldl joinTriggers tOk $ matchingTriggers beforeCreate
      after  = foldl joinTriggers tOk $ matchingTriggers afterCreate

  s <- PS.getPostgresState
  Pool.withResource (PS.pgPool s) $ \pgconn -> do
    liftIO $ PG.beginLevel PG.ReadCommitted pgconn
    (Right _, st1) <- runStateT (evalDsl before) (DslState undefined patch pgconn)
    Right ident    <- liftIO $ Patch.create (st_patch st1) pgconn
    (Right _, st2) <- runStateT (evalDsl after) (st1{st_ident = ident})
    liftIO $ PG.commit pgconn
    return $ Right (st_ident st2, st_patch st2)


runUpdateTriggers
  :: forall m . Model m
  => IdentI m -> Patch m
  -> AppHandler (TriggerRes m)
runUpdateTriggers ident patch = do
  let mName = modelName (modelInfo :: ModelInfo m)
      pName = parentName (modelInfo :: ModelInfo m)
      fields = HM.keys $ untypedPatch patch
      keys = maybe [] (\p -> map (p,) fields) pName ++ map (mName,) fields

      matchingTriggers m = concat $ mapMaybe (`Map.lookup` m) keys

      joinTriggers k tr = do
        let tr' = fromDyn tr $ tError 500 "dynamic BUG"
        tr' >>= either (return . Left) (const k)

      before = foldl joinTriggers tOk $ matchingTriggers beforeUpdate
      after  = foldl joinTriggers tOk $ matchingTriggers afterUpdate

  s <- PS.getPostgresState
  Pool.withResource (PS.pgPool s) $ \pgconn -> do
    liftIO $ PG.beginLevel PG.ReadCommitted pgconn
    (Right _, st1) <- runStateT (evalDsl before) (DslState ident patch pgconn)
    Right count    <- liftIO $ Patch.update ident (st_patch st1) pgconn
    case count of
      0 -> return $ Left (404, "no such object " ++ show ident)
      _ -> do
        (Right _, st2) <- runStateT (evalDsl after)  st1
        liftIO $ PG.commit pgconn
        return $ Right $ st_patch st2


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

    nobody = HaskellE $ return Nothing

    currentUser =
        HaskellE $
        Just <$> toHaskell (BO.userField Usermeta.ident)

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
      \ctx -> modifyPatch (Patch.delete acc) >> evalHaskell ctx body

    setServiceField acc v =
        HaskellE $ do
          ctx <- ask
          return $ do
            let sid = fromMaybe (error "No service in context") $
                      (`get'` Service.ident) <$> service ctx
                p = Patch.put acc (evalHaskell ctx v) Patch.empty
            void $ dbUpdate sid p

    sendDealerMail = HaskellE $ return $ return ()

    sendGenserMail = HaskellE $ return $ return ()

    sendPSAMail = HaskellE $ return $ return ()

    sendSMS _ = HaskellE $ return $ return ()

    closeWith types res =
        HaskellE $ do
          ctx <- ask
          return $ do
            let cid = kase ctx `get'` Case.ident
                p = Patch.put Action.result (Just res) Patch.empty
            aids <- prevClosedActions cid types
            forM_ aids (\(PS.Only i) -> dbUpdate i p)

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
            let cid = kase ctx `get'` Case.ident
                sid = (`get'` Service.ident) <$> service ctx
                -- Never breaks for a valid back office
                e = fromMaybe (error "Target action unknown") $
                    find ((== aT) . BO.aType) $
                    snd carmaBackoffice
                -- Set current action as a source in the nested
                -- evaluator context
                ctx' = ctx{prevAction = Just aT}
                grp = evalHaskell ctx' $ BO.targetRole e
                who = evalHaskell ctx' $ BO.assignment e
                due = evalHaskell ctx' $ BO.due e
                -- Set assignTime if a user is picked
                ctime = now ctx
                assTime = maybe Nothing (const $ Just ctime) who
                p = Patch.put Action.ctime ctime $
                    Patch.put Action.duetime due $
                    Patch.put Action.targetGroup grp $
                    Patch.put Action.assignedTo who $
                    Patch.put Action.assignTime assTime $
                    Patch.put Action.aType aT $
                    Patch.put Action.caseId cid $
                    Patch.put Action.serviceId sid
                    Patch.empty
            dbCreate p >> evalHaskell ctx (BO.proceed ts)

    defer =
        HaskellE $ do
          ctx <- ask
          return $ do
            this <- dbRead =<< getIdent
            let aT = this `get'` Action.aType
                cid = kase ctx `get'` Case.ident
                sid = (`get'` Service.ident) <$> service ctx
                -- Never breaks for a valid back office
                e = fromMaybe (error "Current action unknown") $
                    find ((== aT) . BO.aType) $
                    snd carmaBackoffice
                -- Set current action as a source in the nested
                -- evaluator context
                ctx' = ctx{prevAction = Just aT}
                grp = evalHaskell ctx' $ BO.targetRole e
                HMDiffTime deferBy =
                  fromMaybe (error "No deferBy in action") $
                  this `get'` Action.deferBy
                -- Truncate everything below seconds, disregard leap
                -- seconds
                deferBy' = realToFrac deferBy
                due = addUTCTime deferBy' (now ctx)
                p = Patch.put Action.ctime (now ctx) $
                    Patch.put Action.duetime due $
                    Patch.put Action.targetGroup grp $
                    Patch.put Action.aType aT $
                    Patch.put Action.caseId cid $
                    Patch.put Action.serviceId sid
                    Patch.empty
            void $ dbCreate p


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


mkContext :: PreContextAccess m => Maybe ActionTypeI -> Free (Dsl m) HCtx
mkContext act = do
  srv <- getService
  kase' <- getKase
  usr <- dbRead =<< currentUserId
  t <- getNow
  return HCtx{ kase = kase'
             , service = srv
             , user = usr
             , prevAction = act
             , now = t
             }


evalHaskell :: HCtx -> HaskellE ty -> HaskellType ty
evalHaskell c t = runReader (toHaskell t) c


-- | Bootstrapping HaskellE context.
emptyContext :: HCtx
emptyContext = error "Empty context accessed (HaskellE interpreter bug)"


data HCtx =
    HCtx { kase       :: Patch Case
         , user       :: Patch Usermeta
         , service    :: Maybe (Patch Service)
         , prevAction :: Maybe ActionTypeI
         , now        :: UTCTime
         -- ^ Frozen time.
         }
