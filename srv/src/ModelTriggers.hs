{-# LANGUAGE ScopedTypeVariables #-}

module ModelTriggers
  (runCreateTriggers
  ,runUpdateTriggers
  ) where


import Control.Monad.IO.Class (liftIO)
import Control.Monad.Free (Free)
import Control.Monad.Trans.State

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.Dynamic
import GHC.TypeLits

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple.Transaction as PG
import qualified Snap.Snaplet.PostgresqlSimple as PS

import Application (AppHandler)
import Data.Model as Model
import Data.Model.Patch as Patch (Patch, put, untypedPatch)
import qualified Data.Model.Patch.Sql as Patch

import Trigger.Dsl
import qualified Carma.Model.Usermeta as Usermeta
import           Carma.Model.Call (Call)
import qualified Carma.Model.Call as Call


-- TODO: rename
--   - trigOnModel -> onModel :: ModelCtr m c => c -> Free (Dsl m) res
--   - trigOnField -> onField


beforeCreate :: Map ModelName [Dynamic]
beforeCreate = Map.unionsWith (++)
  [trigOnModel ([]::[Call]) $ do
    uid <- currentUserId
    modifyPatch $ Patch.put Call.callTaker uid
  ]

afterCreate :: Map ModelName [Dynamic]
afterCreate = Map.unionsWith (++)
  []


runUpdateTriggers
  :: forall m . Model m
  => IdentI m -> Patch m -> AppHandler (TriggerRes m)
runUpdateTriggers = runFieldTriggers $ Map.unionsWith (++)
  [trigOn Usermeta.delayedState $ \_ -> wsMessage
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
  . (Model m, SingI name, Typeable typ)
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
