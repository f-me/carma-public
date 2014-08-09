{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, DeriveFunctor #-}

module Trigger.Dsl where

import Control.Applicative
import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import Data.Int (Int64)
import Data.Typeable
import GHC.TypeLits

import qualified Snap.Snaplet.PostgresqlSimple as PG
import qualified Data.Pool as Pool

import Application (AppHandler)
import Data.Model as Model
import Data.Model.Patch (Patch)
import qualified Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch

import Snaplet.Messenger (sendMessage)
import Snaplet.Messenger.Class (withMsg)
import Utils.LegacyModel (mkLegacyIdent)
import Utils.Events (logLegacyCRUD)
import Carma.Model.Event (EventType(Update))


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

dbCreate :: Model m => Patch m -> Free (Dsl n) (IdentI m)
dbCreate p = liftFree (DbCreate p id)

dbUpdate :: Model m => IdentI m -> Patch m -> Free (Dsl n) Int64
dbUpdate i p = liftFree (DbUpdate i p id)

wsMessage :: Free (Dsl m) ()
wsMessage = liftFree (WsMessage ())

logLegacy
  :: (Model m, SingI name, Typeable typ)
  => (m -> F typ name opt)
  -> Free (Dsl m) ()
logLegacy fld = liftFree (LogLegacy fld ())


liftFree :: Functor f => f a -> Free f a
liftFree = Free . fmap Pure


-- These are internals of our DSL
----------------------------------------------------------------------

-- Core DSL operations
-- Don't use them in triggers, use wrappers defined in the section above.
-- Add more if required but try to keep list of core operations small, this
-- will simplify DSL interpreter.
data Dsl m k where
  GetPatch :: (Patch m -> k) -> Dsl m k
  GetIdent :: (IdentI m -> k) -> Dsl m k
  DbCreate :: Model m1 => Patch m1 -> (IdentI m1 -> k) -> Dsl m k
  DbUpdate :: Model m1 => IdentI m1 -> Patch m1 -> (Int64 -> k) -> Dsl m k
  WsMessage:: k -> Dsl m k
  LogLegacy
    :: (Model m, SingI name, Typeable typ)
    => (m -> F typ name opt) -> k -> Dsl m k

deriving instance Typeable2 Dsl

-- deriving instance Functor  (Dsl m)
-- seems we can do this automatically in GHC 7.8
-- https://ghc.haskell.org/trac/ghc/ticket/8678
instance Functor (Dsl m) where
  fmap fn = \case
    GetPatch      k -> GetPatch      $ fn . k
    GetIdent      k -> GetIdent      $ fn . k
    DbCreate  p   k -> DbCreate  p   $ fn . k
    DbUpdate  i p k -> DbUpdate  i p $ fn . k
    WsMessage     k -> WsMessage     $ fn k
    LogLegacy f   k -> LogLegacy f   $ fn k


data DbUpdateArgs where
  DbUpdateArgs :: Model m => IdentI m -> Patch m -> DbUpdateArgs

data DslState m = DslState
  { st_ident :: IdentI m
  , st_patch :: Patch m
  }

-- Our Dsl is evaluated in @AppHandler@ context, so it have access to IO and
-- Snap's state.
-- TODO: create transaction
evalDsl
  :: forall m res . Model m
  => Free (Dsl m) res -> StateT (DslState m) AppHandler res
evalDsl = \case
  Pure res -> return res
  Free op  -> case op of
    GetIdent k -> gets st_ident >>= evalDsl . k
    GetPatch k -> gets st_patch >>= evalDsl . k
    DbCreate p k -> do
      Right res <- lift $ do
        s <- PG.getPostgresState
        Pool.withResource (PG.pgPool s) (liftIO . Patch.create p)
      evalDsl $ k res
    DbUpdate i p k -> do
      Right res <- lift $ do
        s <- PG.getPostgresState
        Pool.withResource (PG.pgPool s) (liftIO . Patch.update i p)
      evalDsl $ k res
    WsMessage k -> do
      -- FIXME: Applicative style
      p <- gets st_patch
      i <- mkLegacyIdent <$> gets st_ident
      lift $ withMsg $ sendMessage i p
      evalDsl k
    LogLegacy fld k -> do
      i <- mkLegacyIdent <$> gets st_ident
      lift $ logLegacyCRUD Update i fld
      evalDsl k
