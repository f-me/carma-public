{-# LANGUAGE ScopedTypeVariables #-}

module ModelTriggers
  (runCreateTriggers
  ,runUpdateTriggers
  ) where


import Control.Applicative
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
import Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch

import Trigger.Dsl
import           Carma.Model.Usermeta (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta
import qualified Carma.Model.BusinessRole as BusinessRole
import           Carma.Model.Call (Call)
import qualified Carma.Model.Call as Call


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
  ]

afterUpdate :: Map (ModelName, FieldName) [Dynamic]
afterUpdate = Map.unionsWith (++)
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


-- | This is how we make new trigger
trigOn
  :: forall m name typ desc app res
  . (Model m, SingI name, Typeable typ)
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

      matchingTriggers m = concat $ catMaybes $ map (`Map.lookup` m) keys

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
