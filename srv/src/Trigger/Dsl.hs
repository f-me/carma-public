{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}

module Trigger.Dsl where

import Control.Applicative
import Control.Monad.Free
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import Data.Typeable
import GHC.TypeLits

import Application (AppHandler)
import Data.Model as Model
import Data.Model.Patch (Patch)
import qualified Data.Model.Patch as Patch

import Trigger.Dsl.SendWsMessage


type TriggerRes m = Either (Int,String) (Patch m)


-- DSL operations
----------------------------------------------------------------------

getPatch :: Free (Dsl m) (Patch m)
getPatch = liftFree (GetPatch id)


getPatchField
  :: (KnownSymbol name, Typeable typ)
  => (m -> Field typ (FOpt name desc app))
  -> Free (Dsl m) (Maybe typ)
getPatchField fld = (`Patch.get` fld) <$> getPatch


tError :: Int -> String -> Free (Dsl m) (TriggerRes m)
tError httpCode msg = return $ Left (httpCode, msg)


tOk :: Free (Dsl m) (TriggerRes m)
tOk = Right <$> getPatch

-- FIXME: we need better name for this operation
sendWsMessage
  :: (Model m, KnownSymbol name, Typeable typ)
  => (m -> F typ name opt)
  -> Free (Dsl m) ()
sendWsMessage fld = liftFree (WsMessage (WsMessageField fld) ())


liftFree :: Functor f => f a -> Free f a
liftFree = Free . fmap Pure


-- These are internals of our DSL
----------------------------------------------------------------------

-- Core DSL operations
-- Don't use them in triggers, use wrappers defined in the section above.
-- Add more if required but try to keep list of core operations small, this
-- will simplify DSL interpreter.
data Dsl m k
  = GetPatch (Patch m -> k)
  | WsMessage (WsMessageField m) k
  deriving (Typeable, Functor)


data DslState m = DslState
  { st_ident :: IdentI m
  , st_patch :: Patch m
  }

-- Our Dsl is evaluated in @AppHandler@ context, so it have access to IO and
-- Snap's state.
evalDsl
  :: forall m res . Model m
  => Free (Dsl m) res -> StateT (DslState m) AppHandler res
evalDsl = \case
  Pure res -> return res
  Free (GetPatch k) -> gets st_patch >>= evalDsl . k
  Free (WsMessage (WsMessageField fld) k) -> do
    p <- gets st_patch
    i <- gets st_ident
    lift $ sendWsMsg fld i p
    evalDsl k
