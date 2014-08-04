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

import Backoffice hiding (const)


runUpdateTriggers
  :: forall m . Model m
  => IdentI m -> Patch m -> AppHandler (TriggerRes m)
runUpdateTriggers = runTriggers $ Map.unionsWith (++)
  ([ trigOn Usermeta.delayedState $ \_ -> sendWsMessage Usermeta.delayedState ] ++
   (map entryToTrigger $ fst carmaBackoffice))

entryToTrigger e = toHaskell (trigger e)

newtype HaskellE (e :: Effects) t = HaskellE (HaskellType t)

type family HaskellType t

type instance HaskellType Trigger = Map (ModelName, FieldName) [Dynamic]

instance Backoffice HaskellE where
    onServiceField' acc f =
        HaskellE $ trigOn acc $ \_ -> undefined


toHaskell :: HaskellE e t -> HaskellType t
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
      tr' >>= either (return . Left) (const k)
