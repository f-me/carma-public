{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

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

      -- ** Snap auth access
    , getCurrentUser
    , createSnapUser
    , updateSnapUserFromUsermeta

    , tError
    , tOk

      -- ** Database access
    , dbCreate
    , dbRead
    , dbUpdate
    , prevClosedActions

      -- ** Miscellaneous
    , wsMessage
    , getNow
    )

where

import Control.Applicative
import Control.Monad
import Control.Exception (SomeException)
import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Time.Calendar.Julian
import Data.Time.Clock
import Data.Int (Int64)
import Data.Typeable
import GHC.TypeLits

import Snap.Snaplet.Auth
import Snaplet.Auth.Class

import Application (AppHandler)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ
import Data.Model as Model
import Data.Model.Patch (Patch)
import qualified Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch

import Snaplet.Messenger (sendMessage)
import Snaplet.Messenger.Class (withMsg)
import Utils.LegacyModel (mkLegacyIdent)

import qualified Carma.Model.Action as Action
import Carma.Model.ActionType  (ActionType)
import Carma.Model.Case        (Case)
import Carma.Model.Usermeta    (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta
import Carma.Model.LegacyTypes (Password(..))

import Util

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
  :: (KnownSymbol name, Typeable typ)
  => (m -> Field typ (FOpt name desc app))
  -> Free (Dsl m) (Maybe typ)
getPatchField fld = (`Patch.get` fld) <$> getPatch


getCurrentUser :: Free (Dsl m) (IdentI Usermeta)
getCurrentUser = liftFree (CurrentUser id)

createSnapUser :: Text -> Free (Dsl Usermeta) ()
createSnapUser login = do
  uid <- liftFree (CreateUser login id)
  modifyPatch (Patch.put Usermeta.uid uid)

updateSnapUserFromUsermeta :: Free (Dsl Usermeta) ()
updateSnapUserFromUsermeta = do
  maybePass <- getPatchField Usermeta.password
  u <- getIdent >>= dbRead
  let u' = maybe u (\p -> Patch.put Usermeta.password p u) maybePass
  liftFree (UpdateUser u' ())


tError :: Int -> String -> Free (Dsl m) (TriggerRes m)
tError httpCode msg = return $ Left (httpCode, msg)

tOk :: Free (Dsl m) (TriggerRes m)
tOk = Right <$> getPatch


dbCreate :: Model m => Patch m -> Free (Dsl n) (IdentI m)
dbCreate p = liftFree (DbCreate p id)

dbRead :: Model m => IdentI m -> Free (Dsl n) (Patch m)
dbRead i = liftFree (DbRead i id)

dbUpdate :: Model m => IdentI m -> Patch m -> Free (Dsl n) Int64
dbUpdate i p = liftFree (DbUpdate i p id)

wsMessage :: Free (Dsl m) ()
wsMessage = liftFree (WsMessage ())

currentUserId :: Free (Dsl m) (IdentI Usermeta)
currentUserId = liftFree (CurrentUser id)

getNow :: Free (Dsl m) UTCTime
getNow = liftFree (DoIO getCurrentTime id)

-- | List of closed actions in a case.
prevClosedActions :: IdentI Case
                  -> [IdentI ActionType]
                  -- ^ Select only actions of this type.
                  -> Free (Dsl m) [PG.Only (IdentI Action.Action)]
prevClosedActions cid types =
    liftFree (DbQuery (\c -> PG.query c q params) id)
    where
      q = [sql|
           SELECT ? FROM ?
           WHERE ? = ?
           AND ? IN ?
           AND ? IS NOT NULL
           ORDER BY ? DESC;
           |]
      params = ( fieldPT Action.ident
               , tableQT Action.ident
               , fieldPT Action.caseId
               , cid
               , fieldPT Action.aType
               , PG.In types
               , fieldPT Action.result
               , fieldPT Action.closeTime
               )

liftFree :: Functor f => f a -> Free f a
liftFree = Free . fmap Pure


-- These are internals of our DSL
----------------------------------------------------------------------

-- Core DSL operations
-- Don't use them in triggers, use wrappers defined in the section above.
-- Add more if required but try to keep list of core operations small, this
-- will simplify DSL interpreter.
data Dsl m k where
  GetPatch    :: (Patch m -> k) -> Dsl m k
  ModPatch    :: (Patch m -> Patch m) -> k -> Dsl m k
  GetIdent    :: (IdentI m -> k) -> Dsl m k
  DbCreate    :: Model m1 => Patch m1 -> (IdentI m1 -> k) -> Dsl m k
  DbRead      :: Model m1 => IdentI m1 -> (Patch m1 -> k) -> Dsl m k
  DbUpdate    :: Model m1 => IdentI m1 -> Patch m1 -> (Int64 -> k) -> Dsl m k
  DbQuery     :: (PG.Connection -> IO [res]) -> ([res] -> k) -> Dsl m k
  CurrentUser :: (IdentI Usermeta -> k) -> Dsl m k
  CreateUser  :: Text -> (Int -> k) -> Dsl m k
  UpdateUser  :: Patch Usermeta -> k -> Dsl m k
  WsMessage   :: k -> Dsl m k
  DoIO        :: IO r -> (r -> r') -> Dsl m r'

deriving instance Typeable Dsl

-- deriving instance Functor  (Dsl m)
-- seems we can do this automatically in GHC 7.8
-- https://ghc.haskell.org/trac/ghc/ticket/8678
instance Functor (Dsl m) where
  fmap fn = \case
    GetPatch      k -> GetPatch      $ fn . k
    ModPatch  f   k -> ModPatch  f   $ fn   k
    GetIdent      k -> GetIdent      $ fn . k
    DbCreate  p   k -> DbCreate  p   $ fn . k
    DbRead    i   k -> DbRead i      $ fn . k
    DbUpdate  i p k -> DbUpdate  i p $ fn . k
    DbQuery   q   k -> DbQuery q     $ fn . k
    CurrentUser   k -> CurrentUser   $ fn . k
    CreateUser l  k -> CreateUser l  $ fn . k
    UpdateUser p  k -> UpdateUser p  $ fn   k
    WsMessage     k -> WsMessage     $ fn   k
    DoIO       a  k -> DoIO       a  $ fn . k


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
    DbQuery q k    -> do
      c <- gets st_pgcon
      res <- liftIO $ q c
      evalDsl $ k res

    CurrentUser k -> do
      c <- gets st_pgcon
      [[uid]] <- lift $ do
        Just u <- withAuth currentUser
        liftIO $ PG.query c
          "SELECT id FROM usermetatbl WHERE uid = ? :: int"
          (PG.Only $ unUid <$> userId u)
      evalDsl $ k (Ident uid)

    CreateUser l k -> do
      let user = defAuthUser {userLogin = l}
      res <- lift $ withAuth $ withBackend $ \bk -> liftIO $ save bk user
      case res of
        Left e -> error $ "Could not create new user: " ++ show e
        Right newUser -> do
          let Just (UserId uid) = userId newUser
          case T.decimal uid of
            Right (uidNum,"") -> evalDsl $ k uidNum
            _ -> error "Impossible in CreateUser"

    UpdateUser u k -> do
      let Just login    = Patch.get u Usermeta.login
          Just isActive = Patch.get u Usermeta.isActive
          Just uid      = Patch.get u Usermeta.uid
          maybePwd      = Patch.get u Usermeta.password
      lift $ do
        let uid' = UserId $ T.pack $ show uid -- FIXME: Usermeta.uid :: F Text
        Just user <- withAuth $ withBackend
          $ \bk -> liftIO $ lookupByUserId bk uid'
        let zabriskiePoint = UTCTime (fromJulian 3001 0 0) 0
            lock = if isActive then Nothing else Just zabriskiePoint
            user' = user {userLogin = login, userLockedOutUntil = lock}
        user'' <- maybe
          (return user')
          (\(Password pwd) -> liftIO $ setPassword user' (T.encodeUtf8 pwd))
          maybePwd
        void $ withAuth $ withBackend -- FIXME: can fail
          $ \bk -> liftIO $ save bk user''
      evalDsl k

    WsMessage k -> do
      p <- gets st_patch
      i <- mkLegacyIdent <$> gets st_ident
      lift $ withMsg $ sendMessage i p
      evalDsl k

    DoIO a k -> evalDsl =<< k <$> liftIO a
