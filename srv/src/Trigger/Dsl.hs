{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}

module Trigger.Dsl where

import Control.Applicative
import Control.Monad.Free
import Control.Monad.Trans.State

import Data.Typeable
import GHC.TypeLits

import Application (AppHandler)
import Data.Model as Model
import Data.Model.Patch (Patch)
import qualified Data.Model.Patch as Patch


type TriggerRes m = Either (Int,String) (Patch m)


-- DSL operations
----------------------------------------------------------------------

getPatch :: Free (Dsl m) (Patch m)
getPatch = liftFree (GetPatch id)


getPatchField
  :: (SingI name, Typeable typ)
  => (m -> Field typ (FOpt name desc app))
  -> Free (Dsl m) (Maybe typ)
getPatchField fld = (`Patch.get` fld) <$> getPatch


tError :: Int -> String -> Free (Dsl m) (TriggerRes m)
tError httpCode msg = return $ Left (httpCode, msg)


tOk :: Free (Dsl m) (TriggerRes m)
tOk = Right <$> getPatch


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
  -- update patch (only some fields, it will be awkward to replace it completely)
  -- syslog message
  -- status change
  -- db query
  -- get/modify some related object from db
  -- ...
  deriving (Typeable, Functor)


data DslState m = DslState
  { st_ident :: IdentI m
  , st_patch :: Patch m
  }

-- Our Dsl is evaluated in @AppHandler@ context, so it have access to IO and
-- Snap's state.
evalDsl :: Free (Dsl m) k -> StateT (DslState m) AppHandler k
evalDsl = \case
  Free (GetPatch k) -> gets st_patch >>= evalDsl . k
  Pure res -> return res
