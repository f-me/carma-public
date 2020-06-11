{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}

module Triggers.DSL
    (
      -- * DSL evaluator
      Dsl
    , DslState(..)
    , DslM, runDslM
    , evalDsl
    , inParentContext

      -- * DSL terms
      -- ** Context access
    , getIdent
    , putIdentUnsafe
    , getPatch
    , modifyPatch
    , getPatchField

      -- ** Snap auth access
    , getCurrentUser
    , createSnapUser
    , updateSnapUserFromUsermeta

      -- ** Database access
    , dbCreate
    , dbRead
    , dbUpdate
    , caseActions
    , callActionIds

      -- ** Miscellaneous
    , userIsReady
    , logCRUDState
    , wsMessage
    , getNow
    , getCityWeather

    -- ** Embed arbitrary AppHandler action into DSL
    , doApp
    )

where

import Control.Monad
import Control.Exception (SomeException)
import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Control.Monad.Trans.State hiding (gets)
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
import Snaplet.Auth.PGUsers

import WeatherApi (Weather, getWeather')

import Application (AppHandler, weatherCfg)

import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap.Snaplet.PostgresqlSimple as PS


import Data.Model as Model
import Data.Model.Patch (Object, Patch)
import qualified Data.Model.Sql as Sql
import qualified Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch
import Data.Model.Utils.LegacyModel (mkIdentTopic)

import Snaplet.Messenger (sendMessage)
import Snaplet.Messenger.Class (withMsg)

import qualified Carma.Model.Action as Action
import qualified Carma.Model.Call   as Call
import Carma.Model.Case        (Case)
import Carma.Model.Event       (Event, EventType)
import qualified Carma.Model.Usermeta as Usermeta
import Carma.Model.LegacyTypes (Password(..))

import qualified AppHandlers.Users as Users
import qualified Utils.Events as Evt (logCRUDState)


-- DSL operations
----------------------------------------------------------------------

getIdent :: Free (Dsl m) (IdentI m)
getIdent = liftFree $ ModState (\st -> (st, st_ident st)) id

putIdentUnsafe :: IdentI m -> DslM m ()
putIdentUnsafe i = liftFree $ ModState (\st -> (st{st_ident = i}, ())) id

getPatch :: Free (Dsl m) (Patch m)
getPatch = liftFree $ ModState (\st -> (st, st_patch st)) id

modifyPatch :: (Patch m -> Patch m) -> Free (Dsl m) ()
modifyPatch f = liftFree
  $ ModState (\st -> (st{st_patch = f (st_patch st)}, ())) id

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

dbCreate :: Model m => Patch m -> Free (Dsl n) (IdentI m)
dbCreate p = liftFree (DbCreate p id)

dbRead :: (PG.FromRow (p m), Model m) => IdentI m -> Free (Dsl n) (p m)
dbRead i = liftFree (DbRead i id)

dbUpdate :: Model m => IdentI m -> Patch m -> Free (Dsl n) Int64
dbUpdate i p = liftFree (DbUpdate i p id)

wsMessage :: Free (Dsl m) ()
wsMessage = liftFree (WsMessage ())

getNow :: Free (Dsl m) UTCTime
getNow = liftFree (DoApp (liftIO getCurrentTime) id)

userIsReady :: IdentI Usermeta -> Free (Dsl m) Bool
userIsReady uid = liftFree (DoApp (Users.userIsReady uid) id)

logCRUDState :: Model m =>
                EventType
             -> IdentI m
             -> Patch m
             -> Free (Dsl m1) (IdentI Event)
logCRUDState m i p =
  liftFree (DoApp (Evt.logCRUDState m i p) id)


doApp :: AppHandler a -> Free (Dsl m) a
doApp f = liftFree (DoApp f id)

inParentContext
  :: (Model m, p ~ Parent m, Model p)
  => DslM p () -> DslM m ()
inParentContext act = do
  let getState = liftFree $ ModState (\st -> (st, st)) id
  let setState s = liftFree $ ModState (const (s, ())) id
  st <- getState
  let st_p = st
        {st_ident = Patch.toParentIdent $ st_ident st
        ,st_patch = Patch.toParentPatch $ st_patch st
        }
  Right st_p' <- doApp $ runDslM' st_p act
  let patch' = Patch.mergeParentPatch (st_patch st) (st_patch st_p')
  setState $ st {st_patch = patch'}


-- | List of actions in a case and all calls for this case, ordered by
-- 'Action.closeTime' and 'Action.ctime' in descending order (latest
-- come first).
caseActions :: IdentI Case
            -> Free (Dsl m) [Object Action.Action]
caseActions cid = doApp $ PS.liftPG' $
  \conn ->
    PG.query conn
      [sql|
        with caseActions as
          ( (select * from actiontbl where caseId = ?)
            union
            (select a.*
              from actiontbl a, calltbl c
              where a.callId = c.id and c.caseId = ?)
          )
          select distinct * from caseActions
            order by closeTime desc, ctime desc
      |] (cid, cid)


callActionIds :: IdentI Call.Call
              -> Free (Dsl m) [IdentI Action.Action]
callActionIds cid =
  liftFree $
  DbIO (Sql.select
    (  Action.ident
    :. Action.callId `Sql.eq` Just cid
    :. Sql.isNull Action.result
    ))
    $ map (\(PS.Only x :. _) -> x)


getCityWeather :: Text -> Free (Dsl m) (Either String Weather)
getCityWeather city = liftFree (DoApp action id)
  where
    action :: AppHandler (Either String Weather)
    action = do
      conf <- gets weatherCfg
      weather <- liftIO $ getWeather' conf
                 $ T.unpack (T.filter (/= '\'') city) ++ ",ru"
      return $ case weather of
                 Right w -> Right w
                 Left e  -> Left $ show e

liftFree :: Functor f => f a -> Free f a
liftFree = Free . fmap Pure


-- These are internals of our DSL
----------------------------------------------------------------------

-- Core DSL operations
-- Don't use them in triggers, use wrappers defined in the section above.
-- Add more if required but try to keep list of core operations small, this
-- will simplify DSL interpreter.
data Dsl m k where
  ModState    :: (DslState m -> (DslState m, res)) -> (res -> k) -> Dsl m k
  DbCreate    :: Model m1 => Patch m1 -> (IdentI m1 -> k) -> Dsl m k
  DbRead      :: (PG.FromRow (p m1), Model m1) =>
                 IdentI m1 -> (p m1 -> k) -> Dsl m k
  DbUpdate    :: Model m1 => IdentI m1 -> Patch m1 -> (Int64 -> k) -> Dsl m k
  DbIO        :: (PG.Connection -> IO res) -> (res -> k) -> Dsl m k
  CurrentUser :: (IdentI Usermeta -> k) -> Dsl m k
  CreateUser  :: Text -> (Int -> k) -> Dsl m k
  UpdateUser  :: Patch Usermeta -> k -> Dsl m k
  WsMessage   :: k -> Dsl m k
  DoApp       :: AppHandler r -> (r -> r') -> Dsl m r'
  ConsistencyBUG :: String -> Dsl m a
  -- ^ This should be used only in really exceptional cases as it can breake
  -- transaction transparency.

deriving instance Functor (Dsl m)

instance MonadFail (Free (Dsl m)) where
  fail = Free . ConsistencyBUG


data DslState m = DslState
  { st_ident :: IdentI m
  , st_patch :: Patch m
  }

type DslM m res = Free (Dsl m) res

-- | DSL evaluation.
runDslM :: Model m =>
           DslState m -> DslM m a -> AppHandler (Either String (DslState m))
runDslM st f = PS.withTransactionLevel PS.ReadCommitted $ runDslM' st f

-- | Non-transactioned DSL evaluation.
runDslM' :: Model m =>
            DslState m -> DslM m a -> AppHandler (Either String (DslState m))
runDslM' st f = Right <$> execStateT (evalDsl f) st

runDb
  :: Model m
  => (PG.Connection -> IO (Either SomeException res))
  -> (res -> Free (Dsl m) res')
  -> StateT (DslState m) AppHandler res'
runDb f k = (lift $ PS.liftPG' f) >>= \case
  Right res -> evalDsl $ k res
  Left err  -> error $ show err -- FIXME: I don't know what to do


-- Our Dsl is evaluated in @AppHandler@ context, so it has access to IO and
-- Snap's state.
evalDsl
  :: forall m res . Model m
  => Free (Dsl m) res -> StateT (DslState m) AppHandler res
evalDsl = \case
  Pure res -> return res
  Free op  -> case op of
    ModState f k
      -> get >>= \st -> let (st',res) = f st in put st' >> evalDsl (k res)
    DbCreate p k   -> runDb (Patch.create p)   k
    DbRead i k     -> runDb (Patch.read i)     k
    DbUpdate i p k -> runDb (Patch.update i p) k
    DbIO q k -> do
      res <- lift $ PS.liftPG' q
      evalDsl $ k res

    CurrentUser k -> do
      Just uid <- lift currentUserMetaId
      evalDsl $ k uid

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
      i <- mkIdentTopic <$> gets st_ident
      lift $ withMsg $ sendMessage i p
      evalDsl k

    DoApp a k -> evalDsl =<< k <$> lift a

    ConsistencyBUG err -> error $ "ConsistencyBUG: " ++ err
