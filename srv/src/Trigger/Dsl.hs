{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, DeriveFunctor #-}

module Trigger.Dsl
    (
      -- * DSL evaluator
      TriggerRes
    , Dsl
    , DslState(..)
    , evalDsl

      -- * DSL terms
      -- ** Context access
    , getIdent
    , getPatch
    , modifyPatch
    , getPatchField
    , currentUserId

    , tError
    , tOk

      -- ** Database access
    , dbCreate
    , dbRead
    , dbUpdate

      -- ** Miscellaneous
    , wsMessage
    , logLegacy
    )

where

import Control.Applicative
import Control.Exception (SomeException)
import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import Data.Int (Int64)
import Data.Typeable
import GHC.TypeLits

import Snap.Snaplet.Auth
import Snaplet.Auth.Class

import Application (AppHandler)
import qualified Database.PostgreSQL.Simple as PG
import Data.Model as Model
import Data.Model.Patch (Patch)
import qualified Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch

import Snaplet.Messenger (sendMessage)
import Snaplet.Messenger.Class (withMsg)
import Utils.LegacyModel (mkLegacyIdent)
import Utils.Events (logLegacyCRUD)
import Carma.Model.Event (EventType(Update))

import Carma.Model.Usermeta (Usermeta)


type TriggerRes m = Either (Int,String) (Patch m)


-- DSL operations
----------------------------------------------------------------------

getIdent :: Free (Dsl m) (IdentI m)
getIdent = liftFree (GetIdent id)

getPatch :: Free (Dsl m) (Patch m)
getPatch = liftFree (GetPatch id)

modifyPatch :: (Patch m -> Patch m) -> Free (Dsl m) ()
modifyPatch f = liftFree (ModPatch f ())

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

dbRead :: Model m => IdentI m -> Free (Dsl n) (Patch m)
dbRead p = liftFree (DbRead p id)

dbUpdate :: Model m => IdentI m -> Patch m -> Free (Dsl n) Int64
dbUpdate i p = liftFree (DbUpdate i p id)

wsMessage :: Free (Dsl m) ()
wsMessage = liftFree (WsMessage ())

logLegacy
  :: (Model m, SingI name, Typeable typ)
  => (m -> F typ name opt)
  -> Free (Dsl m) ()
logLegacy fld = liftFree (LogLegacy fld ())


currentUserId :: Free (Dsl m) (IdentI Usermeta)
currentUserId = liftFree (CurrentUserId id)


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
  ModPatch :: (Patch m -> Patch m) -> k -> Dsl m k
  GetIdent :: (IdentI m -> k) -> Dsl m k
  DbCreate :: Model m1 => Patch m1 -> (IdentI m1 -> k) -> Dsl m k
  DbRead   :: Model m1 => IdentI m1 -> (Patch m1 -> k) -> Dsl m k
  DbUpdate :: Model m1 => IdentI m1 -> Patch m1 -> (Int64 -> k) -> Dsl m k
  CurrentUserId :: (IdentI Usermeta -> k) -> Dsl m k
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
    ModPatch  f   k -> ModPatch  f   $ fn k
    GetIdent      k -> GetIdent      $ fn . k
    DbCreate  p   k -> DbCreate  p   $ fn . k
    DbRead    i   k -> DbRead i      $ fn . k
    DbUpdate  i p k -> DbUpdate  i p $ fn . k
    CurrentUserId k -> CurrentUserId $ fn . k
    WsMessage     k -> WsMessage     $ fn k
    LogLegacy f   k -> LogLegacy f   $ fn k


data DbUpdateArgs where
  DbUpdateArgs :: Model m => IdentI m -> Patch m -> DbUpdateArgs

data DslState m = DslState
  { st_ident :: IdentI m
  , st_patch :: Patch m
  , st_pgcon :: PG.Connection
  }

runDb
  :: Model m
  => (PG.Connection -> IO (Either SomeException res))
  -> (res -> Free (Dsl m) res')
  -> StateT (DslState m) AppHandler res'
runDb f k = gets st_pgcon >>= liftIO . f >>= \case
  Right res -> evalDsl $ k res
  Left err  -> error $ show err -- FIXME: I don't know what to do

-- Our Dsl is evaluated in @AppHandler@ context, so it have access to IO and
-- Snap's state.
evalDsl
  :: forall m res . Model m
  => Free (Dsl m) res -> StateT (DslState m) AppHandler res
evalDsl = \case
  Pure res -> return res
  Free op  -> case op of
    GetIdent k -> gets st_ident >>= evalDsl . k
    GetPatch k -> gets st_patch >>= evalDsl . k
    ModPatch f k -> do
      modify $ \s -> s {st_patch = f (st_patch s)}
      evalDsl k
    DbCreate p k   -> runDb (Patch.create p)   k
    DbRead i k     -> runDb (Patch.read i)     k
    DbUpdate i p k -> runDb (Patch.update i p) k
    CurrentUserId k -> do
      c <- gets st_pgcon
      [[uid]] <- lift $ do
        Just u <- withAuth currentUser
        liftIO $ PG.query c
          "SELECT id FROM usermetatbl WHERE uid = ? :: int"
          (PG.Only $ unUid <$> userId u)
      evalDsl $ k (Ident uid)
    WsMessage k -> do
      p <- gets st_patch
      i <- mkLegacyIdent <$> gets st_ident
      lift $ withMsg $ sendMessage i p
      evalDsl k
    LogLegacy fld k -> do
      i <- mkLegacyIdent <$> gets st_ident
      lift $ logLegacyCRUD Update i fld
      evalDsl k
