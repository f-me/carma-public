{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

module Trigger.Dsl
    (
      -- * DSL evaluator
      TriggerRes
    , Dsl
    , DslState(..)
    , emptyDslState
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

    , tError
    , tOk

      -- ** Database access
    , dbCreate
    , dbRead
    , dbUpdate
    , prevClosedActions

      -- ** Miscellaneous
    , userIsReady
    , logCRUD
    , wsMessage
    , getNow
    , getCityWeather
    , sendSMS
    )

where

import Control.Applicative
import Control.Monad
import Control.Exception (SomeException)
import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Time.Calendar.Julian
import Data.Time.Clock
import Data.Int (Int64)
import Data.Typeable
import GHC.TypeLits

import qualified Snap (gets)
import Snap.Snaplet.Auth
import Snaplet.Auth.Class
import Snaplet.Auth.PGUsers

import WeatherApi (Weather, getWeather')

import Application (AppHandler, weatherCfg)
import           Database.PostgreSQL.Simple ((:.) (..))
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ
import Data.Model as Model
import Data.Model.Patch (Patch)
import qualified Data.Model.Sql as Sql
import qualified Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch

import Snaplet.Messenger (sendMessage)
import Snaplet.Messenger.Class (withMsg)
import Utils.LegacyModel (mkLegacyIdent)

import qualified Carma.Model.Action as Action
import Carma.Model.ActionType  (ActionType)
import Carma.Model.Case        (Case)
import Carma.Model.Event       (Event, EventType)
import Carma.Model.Service     (Service)
import Carma.Model.SmsTemplate as SmsTemplate
import Carma.Model.Usermeta    (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta
import Carma.Model.LegacyTypes (Password(..))

import qualified AppHandlers.Users as Users
import Util
import qualified Utils.Events as Evt (logCRUD)

type TriggerRes m = Either (Int,String) (Patch m)


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

getNow :: Free (Dsl m) UTCTime
getNow = liftFree (DoApp (liftIO getCurrentTime) id)

userIsReady :: IdentI Usermeta -> Free (Dsl m) Bool
userIsReady uid = liftFree (DoApp (Users.userIsReady uid) id)

logCRUD :: Model m =>
           EventType
        -> IdentI m
        -> Patch m
        -> Free (Dsl m) (IdentI Event)
logCRUD m i p = liftFree (DoApp (Evt.logCRUD m i p) id)

dbQuery :: (PG.FromRow r, PG.ToRow q) => PG.Query -> q -> Free (Dsl m) [r]
dbQuery q params = liftFree $ DbIO (\c -> PG.query c q params) id


inParentContext
  :: (Model m, p ~ Parent m, Model p)
  => DslM p () -> DslM m ()
inParentContext act = do
  let getState = liftFree $ ModState (\st -> (st, st)) id
  let setState s = liftFree $ ModState (const (s, ())) id
  let doApp f = liftFree (DoApp f id)
  st <- getState
  let st_p = st
        {st_ident = Patch.toParentIdent $ st_ident st
        ,st_patch = Patch.toParentPatch $ st_patch st
        }
  Right st_p' <- doApp $ runDslM st_p act
  let patch' = Patch.mergeParentPatch (st_patch st) (st_patch st_p')
  setState $ st {st_patch = patch'}




-- | List of closed actions in a case. Last closed actions come first.
prevClosedActions :: IdentI Case
                  -> [IdentI ActionType]
                  -- ^ Select only actions of this type.
                  -> Free (Dsl m) [PG.Only (IdentI Action.Action)]
prevClosedActions cid types = dbQuery q params
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

getCityWeather :: Text -> Free (Dsl m) (Either String Weather)
getCityWeather city = liftFree (DoApp action id)
  where
    action :: AppHandler (Either String Weather)
    action = do
      conf <- Snap.gets weatherCfg
      weather <- liftIO $ getWeather' conf $
                 T.unpack $ T.filter (/= '\'') city
      return $ case weather of
                 Right w -> Right w
                 Left e  -> Left $ show e

sendSMS :: Model.IdentI Service -> Model.IdentI SmsTemplate -> Free (Dsl m) ()
sendSMS svcId tplId =
  dbQuery q [svcId] >>=
    \case
      [[ caseId, city, phone
       , svcType, eSvcStart, fSvcStart
       , sender, pInfo, pCInfo
       ]] -> do
        let varMap = Map.fromList
              [("program_info", pInfo)
              ,("program_contact_info", pCInfo)
              ,("case.city", city)
              ,("case.id", caseId)
              ,("service.type", svcType)
              ,("service.times_factServiceStart", fSvcStart)
              ,("service.times_expectedServiceStart", eSvcStart)
              ]
        liftFree (DbIO
                  (Sql.select (SmsTemplate.text :.
                               SmsTemplate.ident `Sql.eq` tplId)) id) >>=
          \case
            [PG.Only templateText :. ()] ->
              let
                msg = T.encodeUtf8 $ render varMap templateText
                prms = (caseId, phone, sender, tplId, msg)
              in
                void $ liftFree (DbIO (\c -> PG.execute c qInsert prms) id)
            _ -> return ()
      _ -> return ()
  where
    qInsert =
      [sql|
       insert into "Sms" (caseRef, phone, sender, template, msgText, status)
       values (?, ?, ?, ?, ?, 'please-send')
          |]
    q = [sql|
          select
            cs.id::text,
            coalesce("City".label, ''),
            coalesce(cs.contact_phone1, ''),
            "ServiceType".label,
            coalesce(to_char(svc.times_expectedServiceStart, 'HH24:MI MM-DD-YYYY'), ''),
            coalesce(to_char(svc.times_factServiceStart, 'HH24:MI MM-DD-YYYY'), ''),
            sprog.smsSender, sprog.smsProgram, sprog.smsContact
          from
            casetbl cs left join "City" on ("City".id = cs.city),
            servicetbl svc,
            "ServiceType",
            "SubProgram" sprog
          where true
            and svc.id   = ?
            and svc.type = "ServiceType".id
            and cs.id    = svc.parentId
            and sprog.id = cs.subprogram
        |]


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
  DbRead      :: Model m1 => IdentI m1 -> (Patch m1 -> k) -> Dsl m k
  DbUpdate    :: Model m1 => IdentI m1 -> Patch m1 -> (Int64 -> k) -> Dsl m k
  DbIO        :: (PG.Connection -> IO res) -> (res -> k) -> Dsl m k
  CurrentUser :: (IdentI Usermeta -> k) -> Dsl m k
  CreateUser  :: Text -> (Int -> k) -> Dsl m k
  UpdateUser  :: Patch Usermeta -> k -> Dsl m k
  WsMessage   :: k -> Dsl m k
  DoApp       :: AppHandler r -> (r -> r') -> Dsl m r'

deriving instance Typeable Dsl

-- deriving instance Functor  (Dsl m)
-- seems we can do this automatically in GHC 7.8
-- https://ghc.haskell.org/trac/ghc/ticket/8678
instance Functor (Dsl m) where
  fmap fn = \case
    ModState  f   k -> ModState  f   $ fn . k
    DbCreate  p   k -> DbCreate  p   $ fn . k
    DbRead    i   k -> DbRead i      $ fn . k
    DbUpdate  i p k -> DbUpdate  i p $ fn . k
    DbIO      q   k -> DbIO q        $ fn . k
    CurrentUser   k -> CurrentUser   $ fn . k
    CreateUser l  k -> CreateUser l  $ fn . k
    UpdateUser p  k -> UpdateUser p  $ fn   k
    WsMessage     k -> WsMessage     $ fn   k
    DoApp      a  k -> DoApp      a  $ fn . k


data DslState m = DslState
  { st_ident :: IdentI m
  , st_patch :: Patch m
  , st_pgcon :: PG.Connection
  }

emptyDslState :: IdentI m -> Patch m -> PG.Connection -> DslState m
emptyDslState = DslState

type DslM m res = Free (Dsl m) res

runDslM :: Model m => DslState m -> DslM m a -> AppHandler (Either String (DslState m))
runDslM st f = Right <$> execStateT (evalDsl f) st


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
    ModState f k
      -> get >>= \st -> let (st',res) = f st in put st' >> evalDsl (k res)
    DbCreate p k   -> runDb (Patch.create p)   k
    DbRead i k     -> runDb (Patch.read i)     k
    DbUpdate i p k -> runDb (Patch.update i p) k
    DbIO q k -> do
      c <- gets st_pgcon
      res <- liftIO $ q c
      evalDsl $ k res

    CurrentUser k -> do
      Just uid <- lift $ currentUserMetaId
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
      i <- mkLegacyIdent <$> gets st_ident
      lift $ withMsg $ sendMessage i p
      evalDsl k

    DoApp a k -> evalDsl =<< k <$> lift a
