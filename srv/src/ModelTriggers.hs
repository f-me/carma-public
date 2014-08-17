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

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock
import Data.Dynamic
import GHC.TypeLits

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple.Transaction as PG
import qualified Snap.Snaplet.PostgresqlSimple as PS

import Application (AppHandler)
import Data.Model as Model
import Data.Model.Patch as Patch (Patch, get', put, untypedPatch)
import qualified Data.Model.Patch.Sql as Patch

import           Trigger.Dsl

import           Carma.Model.Call (Call)
import qualified Carma.Model.Call as Call
import           Carma.Model.Case (Case)
import           Carma.Model.Service (Service)
import           Carma.Model.Usermeta (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta

import           Carma.Backoffice.DSL as BO hiding ((==), before, const)
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

runUpdateTriggers
  :: forall m . Model m
  => IdentI m -> Patch m -> AppHandler (TriggerRes m)
runUpdateTriggers = runFieldTriggers $ Map.unionsWith (++)
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
type TriggersMap = Map (ModelName, FieldName) [Dynamic]


-- | This is how we make new trigger
trigOn
  :: forall m name typ desc app res
  . (Model m, KnownSymbol name, Typeable typ)
  => (m -> Field typ (FOpt name desc app)) -- ^ watch this field
  -> (typ -> Free (Dsl m) res)             -- ^ run this if field changed
  -> TriggersMap
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
    -- FIXME: we need to choose isolation level carefully
    liftIO $ PG.beginLevel PG.Serializable pgconn
    (Right _, st1) <- runStateT (evalDsl before) (DslState undefined patch pgconn)
    Right ident    <- liftIO $ Patch.create patch pgconn
    (Right _, st2) <- runStateT (evalDsl after) (st1{st_ident = ident})
    liftIO $ PG.commit pgconn
    return $ Right (st_ident st2, st_patch st2)


runFieldTriggers
  :: forall m . Model m
  => TriggersMap -> IdentI m -> Patch m
  -> AppHandler (TriggerRes m)
runFieldTriggers trMap ident patch = do
  let mName = modelName (modelInfo :: ModelInfo m)
      pName = parentName (modelInfo :: ModelInfo m)
      fields = HM.keys $ untypedPatch patch
      keys = maybe [] (\p -> map (p,) fields) pName ++ map (mName,) fields

      matchingTriggers = concat $ catMaybes $ map (`Map.lookup` trMap) keys
      joinTriggers k tr = do
        let tr' = fromDyn tr $ tError 500 "dynamic BUG"
        tr' >>= either (return . Left) (const k)
  s <- PS.getPostgresState
  Pool.withResource (PS.pgPool s) $ \pgconn -> do
    liftIO $ PG.beginLevel PG.Serializable pgconn
    res <- evalStateT
      (evalDsl $ foldl joinTriggers tOk matchingTriggers)
      (DslState ident patch pgconn)
    liftIO $ PG.commit pgconn
    return res


haskellBinary :: (HaskellType t1 -> HaskellType t2 -> HaskellType t)
              -- ^ Non-lifted binary function.
              -> HaskellE t1
              -> HaskellE t2
              -> HaskellE t
haskellBinary fun = \a b -> HaskellE $ fun <$> toHaskell a <*> toHaskell b


newtype HaskellE t = HaskellE { toHaskell :: Reader HCtx (HaskellType t) }
    deriving Typeable


instance Backoffice HaskellE where
    now = HaskellE $ asks ModelTriggers.now

    since nd t =
        HaskellE $ addUTCTime nd <$> toHaskell t

    role r = HaskellE $ return (Nothing, r)

    currentUserOr r =
        HaskellE $ do
          i <- toHaskell (userField Usermeta.ident)
          return (Just i, r)

    previousAction =
        HaskellE $
        fromMaybe (Ident $ fst startNode) <$> (asks prevAction)

    userField acc =
        HaskellE $ asks (flip Patch.get' acc . user)

    serviceField acc =
        HaskellE $
        asks (flip Patch.get' acc . fromMaybe (error "No service") . service)

    caseField acc =
        HaskellE $
        asks (flip Patch.get' acc . kase)

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
              False -> toHaskell $ switch bs ow
        [] -> toHaskell ow

    not a = HaskellE $ Prelude.not <$> toHaskell a

    (==) = haskellBinary (==)

    (>) = haskellBinary (Prelude.>)

    (&&) = haskellBinary (Prelude.&&)

    (||) = haskellBinary (Prelude.||)

    onField acc target body =
        HaskellE $
        return $
        trigOn acc $
        \newVal -> do
          hctx <- mkContext Nothing
          when (newVal == (evalHaskell hctx target)) (evalHaskell hctx body)

    finish = HaskellE $ return $ return ()


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


data HCtx =
    HCtx { kase       :: Patch Case
         , user       :: Patch Usermeta
         , service    :: Maybe (Patch Service)
         , prevAction :: Maybe ActionTypeI
         , now        :: UTCTime
         -- ^ Frozen time.
         }
