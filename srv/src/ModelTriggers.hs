{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ModelTriggers where

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
import Data.Model.Patch (Patch, untypedPatch)

import Trigger.Dsl

import qualified Carma.Model.Usermeta as Usermeta
import qualified Carma.Model.Call as Call

import Carma.Model.Role (Role)
import Carma.Model.Usermeta (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta

import Carma.Backoffice
import Carma.Backoffice.DSL as BO

runUpdateTriggers
  :: forall m . Model m
  => IdentI m -> Patch m -> AppHandler (TriggerRes m)
runUpdateTriggers = runTriggers $ Map.unionsWith (++)
  ([trigOn Usermeta.delayedState $ \_ -> wsMessage >> logLegacy Usermeta.delayedState
   ,trigOn Call.endDate $ \_ -> logLegacy Call.endDate
   ] ++
   (map entryToTrigger $ fst carmaBackoffice))


entryToTrigger e = toHaskell (trigger e)

newtype HaskellE t = HaskellE (HaskellType t)

type family HaskellType t

type instance HaskellType Trigger = Map (ModelName, FieldName) [Dynamic]

type instance HaskellType Bool = Bool

type instance HaskellType (IdentI m) = (IdentI m)

type instance HaskellType ActionAssignment = (Maybe (IdentI Usermeta), IdentI Role)

type instance HaskellType ActionOutcome = IO ()

instance Backoffice HaskellE where
    const = HaskellE

    role r = HaskellE (Nothing, r)

    not a = HaskellE $ Prelude.not $ toHaskell a

    onServiceField' acc f =
        HaskellE $ trigOn acc $ \_ -> undefined


toHaskell :: HaskellE v -> HaskellType v
toHaskell (HaskellE term) = term


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


-- | This is how we run triggers on a patch
runTriggers
  :: forall m . Model m
  => TriggersMap -> IdentI m -> Patch m
  -> AppHandler (TriggerRes m)
runTriggers trMap ident patch
  = evalStateT
    (evalDsl $ foldl joinTriggers tOk matchingTriggers)
    (DslState ident patch)
  where
    mName = modelName (modelInfo :: ModelInfo m)
    keys = map (mName,) $ HM.keys $ untypedPatch patch
    matchingTriggers = concat $ catMaybes $ map (`Map.lookup` trMap) keys
    joinTriggers k tr = do
      let tr' = fromDyn tr $ tError 500 "dynamic BUG"
      tr' >>= either (return . Left) (Prelude.const k)
