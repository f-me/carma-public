{-# LANGUAGE ScopedTypeVariables #-}

module ModelTriggers
  (runCreateTriggers
  ,runUpdateTriggers
  ) where


import Control.Monad.Free (Free)
import Control.Monad.Trans.State (evalStateT)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.Dynamic
import GHC.TypeLits

import Application (AppHandler)
import Data.Model as Model
import Data.Model.Patch as Patch (Patch, put, untypedPatch)

import Trigger.Dsl
import qualified Carma.Model.Usermeta as Usermeta
import           Carma.Model.Call (Call)
import qualified Carma.Model.Call as Call


runCreateTriggers
  :: forall m . Model m
  => Patch m -> AppHandler (TriggerRes m)
runCreateTriggers = runModelTriggers $ Map.unionsWith (++)
  [trigOnModel ([]::[Call]) $ do
    uid <- currentUserId
    modifyPatch $ Patch.put Call.callTaker uid
  ]


runUpdateTriggers
  :: forall m . Model m
  => IdentI m -> Patch m -> AppHandler (TriggerRes m)
runUpdateTriggers = runFieldTriggers $ Map.unionsWith (++)
  [trigOn Usermeta.delayedState $ \_ -> wsMessage >> logLegacy Usermeta.delayedState
  ,trigOn Call.endDate $ \_ -> logLegacy Call.endDate
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
runModelTriggers
  :: forall m . Model m
  => Map ModelName [Dynamic] -> Patch m
  -> AppHandler (TriggerRes m)
runModelTriggers trMap patch
  = evalStateT
    (evalDsl $ foldl joinTriggers tOk matchingTriggers)
    (DslState undefined patch)
  where
    mName = modelName (modelInfo :: ModelInfo m)
    matchingTriggers = Map.findWithDefault [] mName trMap
    joinTriggers k tr = do
      let tr' = fromDyn tr $ tError 500 "dynamic BUG"
      tr' >>= either (return . Left) (const k)


runFieldTriggers
  :: forall m . Model m
  => TriggersMap -> IdentI m -> Patch m
  -> AppHandler (TriggerRes m)
runFieldTriggers trMap ident patch
  = evalStateT
    (evalDsl $ foldl joinTriggers tOk matchingTriggers)
    (DslState ident patch)
  where
    mName = modelName (modelInfo :: ModelInfo m)
    keys = map (mName,) $ HM.keys $ untypedPatch patch
    matchingTriggers = concat $ catMaybes $ map (`Map.lookup` trMap) keys
    joinTriggers k tr = do
      let tr' = fromDyn tr $ tError 500 "dynamic BUG"
      tr' >>= either (return . Left) (const k)
