{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

{-|

Lower-level database trigger DSL. Ties AppHandler monad and various IO
actions employed in triggers.

-}

module Triggers.DSL
    (
      -- * DSL evaluator
      TriggerRes
    , DslState(..)
    , emptyDslState
    , DslM, runDslM
    , inParentContext

      -- * DSL terms
      -- ** Context access
    , getIdent
    , unsafePutIdent
    , getPatch
    , modifyPatch
    , getPatchField

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
    , caseActions

      -- ** Miscellaneous
    , userIsReady
    , logCRUDState
    , wsMessage
    , getNow

    , inFuture
    )

where

import Control.Applicative
import Control.Monad
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import Data.Maybe
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
import Snaplet.Auth.PGUsers

import Application (AppHandler)
import qualified Database.PostgreSQL.Simple as PG
import           Snap.Snaplet.PostgresqlSimple as PS


import Data.Model as Model
import Data.Model.Patch (Object, Patch)
import qualified Data.Model.Sql as Sql
import qualified Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch

import Snaplet.Messenger (sendMessage)
import Snaplet.Messenger.Class (withMsg)
import Utils.LegacyModel (mkLegacyIdent)

import qualified Carma.Model.Action as Action
import Carma.Model.Case        (Case)
import Carma.Model.Event       (Event, EventType)
import Carma.Model.Usermeta    (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta
import Carma.Model.LegacyTypes (Password(..))

import qualified AppHandlers.Users as Users
import qualified Utils.Events as Evt (logCRUDState)


type TriggerRes m = Either (Int, String) (Patch m)


-- | DSL operations.
data DslState m = DslState
  { st_futur :: [AppHandler (IO ())]
  , st_ident :: IdentI m
  , st_patch :: Patch m
  }


type DslM m res = StateT (DslState m) AppHandler res


getIdent :: DslM m (IdentI m)
getIdent = gets st_ident


unsafePutIdent :: IdentI m -> DslM m ()
unsafePutIdent i = modify $ \s -> s{st_ident = i}


getPatch :: DslM m (Patch m)
getPatch = gets st_patch


modifyPatch :: (Patch m -> Patch m) -> DslM m ()
modifyPatch f = modify $ \s -> s{st_patch = f $ st_patch s}


getPatchField :: (KnownSymbol name, Typeable typ) =>
                 (m -> Field typ (FOpt name desc app))
              -> DslM m (Maybe typ)
getPatchField fld = (`Patch.get` fld) <$> getPatch


getCurrentUser :: DslM m (IdentI Usermeta)
getCurrentUser =
  lift $ (fromMaybe (error "No current user") <$> currentUserMetaId)


createSnapUser :: Text -> DslM Usermeta ()
createSnapUser login = do
  uid <- lift $ do
    let user = defAuthUser {userLogin = login}
    res <- withAuth $ withBackend $ \bk -> liftIO $ save bk user
    case res of
      Left e -> error $ "Could not create new user: " ++ show e
      Right newUser -> do
        let Just (UserId uid) = userId newUser
        case T.decimal uid of
          Right (uidNum, "")
            -> return uidNum
          _ -> error "Impossible in createSnapUser"
  modifyPatch (Patch.put Usermeta.uid uid)


updateSnapUserFromUsermeta :: DslM Usermeta ()
updateSnapUserFromUsermeta = do
  maybePass <- getPatchField Usermeta.password
  u' <- getIdent >>= dbRead
  let u = maybe u' (\p -> Patch.put Usermeta.password p u') maybePass
  lift $ do
      let Just login    = Patch.get u Usermeta.login
          Just isActive = Patch.get u Usermeta.isActive
          Just uid      = Patch.get u Usermeta.uid
          maybePwd      = Patch.get u Usermeta.password
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


tError :: Int -> String -> DslM m (TriggerRes m)
tError httpCode msg = return $ Left (httpCode, msg)


tOk :: DslM m (TriggerRes m)
tOk = Right <$> getPatch


dbCreate :: Model m => Patch m -> DslM m (IdentI m)
dbCreate p = runDb (Patch.create p) id


dbRead :: (PG.FromRow (p n), Model n) => IdentI n -> DslM m (p n)
dbRead i = runDb (Patch.read i) id


dbUpdate :: Model m => IdentI m -> Patch m -> DslM n Int64
dbUpdate i p = runDb (Patch.update i p) id


wsMessage :: Model m => DslM m ()
wsMessage = do
  p <- gets st_patch
  i <- mkLegacyIdent <$> gets st_ident
  lift $ withMsg $ sendMessage i p


getNow :: DslM m UTCTime
getNow = lift $ liftIO getCurrentTime


userIsReady :: IdentI Usermeta -> DslM m Bool
userIsReady uid = lift $ Users.userIsReady uid


logCRUDState :: Model m =>
                EventType
             -> IdentI m
             -> Patch m
             -> DslM m1 (IdentI Event)
logCRUDState m i p = lift $ Evt.logCRUDState m i p


inParentContext
  :: (Model m, p ~ Parent m, Model p)
  => DslM p () -> DslM m ()
inParentContext act = do
  st <- get
  let st_p = st{ st_ident = Patch.toParentIdent $ st_ident st
               , st_patch = Patch.toParentPatch $ st_patch st
               }
  Right st_p' <- lift $ runDslM' st_p act
  let patch' = Patch.mergeParentPatch (st_patch st) (st_patch st_p')
  put st{st_patch = patch'}


-- | List of actions in a case, ordered by 'Action.closeTime' and
-- 'Action.ctime' in descending order (latest come first).
caseActions :: IdentI Case
            -> DslM m [Object Action.Action]
caseActions cid = undefined
  runDb
  (Sql.select (Sql.fullPatch Action.ident :.
                    Action.caseId `Sql.eq` cid :.
                    Sql.descBy Action.closeTime :.
                    Sql.descBy Action.ctime))
  (map (\(x :. ()) -> x))


inFuture :: (AppHandler (IO ())) -> DslM m ()
inFuture f = modify $ \st -> st{st_futur = f : st_futur st}


emptyDslState :: IdentI m -> Patch m -> DslState m
emptyDslState = DslState []


runDslM :: Model m =>
           DslState m
        -> DslM m a
        -> AppHandler (Either String (DslState m))
runDslM st f = PS.withTransactionLevel PS.ReadCommitted $ runDslM st f


runDslM' :: Model m =>
            DslState m
         -> DslM m a
         -> AppHandler (Either String (DslState m))
runDslM' st f = Right <$> execStateT f st


runDb :: (PG.Connection -> IO (Either SomeException res))
      -> (res -> res')
      -> DslM m res'
runDb fetch process =
  lift $
  (liftPG $ \pg -> fetch pg >>= \case
       Right res -> return $ process res
       Left err  -> error $ show err) -- FIXME: I don't know what to do
