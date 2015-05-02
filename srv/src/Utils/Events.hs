{-# LANGUAGE ScopedTypeVariables, QuasiQuotes, FlexibleContexts, DataKinds #-}

module Utils.Events
    (
      logCRUD
    , updateUserState
    , logLogin
    , logCRUDState
    )

where

import           Prelude hiding (log)

import           Control.Monad
import           Control.Monad.RWS
import           Control.Exception (SomeException)

import           Data.String (fromString)
import           Text.Printf

import           Data.Maybe
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (getCurrentTime)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Vector (singleton)

import           GHC.TypeLits

import           Data.Model
import           Data.Model.Patch (Patch)
import qualified Data.Model.Patch     as P
import qualified Data.Model.Patch.Sql as P

import           Snap
import           Snap.Snaplet.Auth
import           Snaplet.Auth.Class
import           Snap.Snaplet.PostgresqlSimple ( liftPG
                                               , Only(..)
                                               , query
                                               )
import           Database.PostgreSQL.Simple.SqlQQ

import           Carma.Model.Event (Event, EventType(..))
import qualified Carma.Model.Event     as E
import           Carma.Model.UserState (UserState, UserStateVal(..))
import qualified Carma.Model.UserState as State
import           Carma.Model.Usermeta  (Usermeta(..))
import qualified Carma.Model.Action as Action
import qualified Carma.Model.Call   as Call

import           Snaplet.Search.Types (mkSel)
import           Snaplet.Messenger
import           Snaplet.Messenger.Class

import           Application
import           AppHandlers.KPI (updateOperKPI)

import           Utils.LegacyModel


-- | Create `Event` for login/logout fact
logLogin :: EventType -> AppHandler ()
logLogin tpe = do
  uid <- getRealUid
  case uid of
    Nothing -> return ()
    Just uid' -> do
      ev <- log $ addIdent uid' $ buildEmpty tpe
      updateUserState tpe uid' P.empty ev

-- | Create 'Event' from changes in a model.
logCRUD :: forall m. Model m =>
           EventType
        -> IdentI m
        -- ^ Identifier of changed model
        -> Patch m
        -- ^ Changed fields
        -> AppHandler (IdentI Event)
logCRUD tpe idt p =
  log $ buildFull tpe idt (Nothing :: Maybe (m -> PK Int m "")) (Just p)

-- | Create event *and* update user state.
logCRUDState :: Model m
             => EventType
             -> IdentI m
             -> Patch m
             -> AppHandler (IdentI Event)
logCRUDState tpe idt p =
  logCRUD tpe idt p >>=
  \e -> updateUserState tpe idt p e >> return e

-- | Create event from patch
log :: Patch Event -> AppHandler (IdentI Event)
log p = do
  uid <- getRealUid
  idt <- create $ setUsr uid p
  case idt of
    Left err -> error $ "Can't create Event: " ++ show err
    Right id' -> return id'

updateUserState :: forall m. Model m =>
                   EventType
                -> IdentI m
                -- ^ Identifier of changed model
                -> Patch m
                -- ^ Changed fields
                -> IdentI Event
                -> AppHandler ()
updateUserState evt idt p evidt = do
  -- Little hack to determine target user for state change in case if
  -- someone else changed @delayedState@ field of current user
  tgtUsr <- case mname of
    -- Rebuild ident so haskell won't complain about m ~ Usermeta
    "Usermeta" -> return $ Just $ Ident $ identVal idt
    _          -> getRealUid
  case tgtUsr of
    Nothing -> return ()
    Just tgtUsr' -> do
      s <- checkUserState tgtUsr' evt evidt idt p
      void $ case s of
        Nothing -> return ()
        Just st -> do
        -- well it's hack of course, current time will be little differene
        -- from real ctime of new state
        time <- liftIO $ getCurrentTime
        withMsg $ sendMessage
          (mkLegacyIdent tgtUsr')
          (P.put currentState      st      $
           P.put currentStateCTime time    $
           P.put delayedState      Nothing $
           P.empty)
        kpis <- updateOperKPI (singleton tgtUsr')
        withMsg $ sendMessage "oper-kpi" kpis
  where
    mname = modelName (modelInfo :: ModelInfo m)


-- Implementation --------------------------------------------------------------

-- | Build `Path Event`
buildFull :: forall m t n d.(Model m, KnownSymbol n)
          => EventType
          -- ^ Type of the event
          -> IdentI m
          -- ^ Identifier of model `m`, emitted event
          -> Maybe (m -> F t n d)
          -- ^ Changed field
          -> Maybe (Patch m)
          -- ^ The whole patch
          -> Patch Event
buildFull tpe idt f patch =
  P.put E.modelId   mid   $
  P.put E.modelName mname $
  P.put E.field     fname $
  P.put E.patch     p'    $
  buildEmpty tpe
   where
     mname = modelName $ (modelInfo :: ModelInfo m)
     mid   = identVal idt
     fname = fieldName <$> f
     p'    = Aeson.toJSON <$> patch

-- | Build `Patch Event` with just user and type
buildEmpty :: EventType -> Patch Event
buildEmpty tpe = P.put E.eventType tpe $  P.empty

-- | Create `Event` in `postgres` from `Patch Event`, return it's id
create :: Patch Event
       -> AppHandler (Either SomeException (IdentI Event))
create ev = with db $ liftPG $ \c -> liftIO $ P.create ev c


data States = States { _from :: [UserStateVal], _to :: UserStateVal }
(>>>) :: [UserStateVal] -> UserStateVal -> States
(>>>) f t = States f t

-- | Check current state and maybe create new
checkUserState :: forall m. Model m
               => IdentI Usermeta
               -> EventType
               -> IdentI Event
               -> IdentI m
               -> Patch m
               -> AppHandler (Maybe UserStateVal)
checkUserState uid evType evIdt _ p = do
  hist <- query
    (fromString (printf
    "SELECT %s FROM \"UserState\" WHERE userId = ? ORDER BY id DESC LIMIT 1"
    (T.unpack $ mkSel (modelInfo :: ModelInfo UserState)))) (Only uid)
  dst <- query [sql| SELECT delayedState FROM usermetatbl where id = ? |]
         (Only uid)
  let delayedSt = head $ head dst
  case hist of
    []          -> setNext $ nextState' LoggedOut delayedSt
    [lastState] -> setNext $ nextState'
                   (P.get' lastState State.state)
                   delayedSt
  where
    nextState' s d =
      let mname = modelName (modelInfo :: ModelInfo m)
      in nextState s d evType mname (HM.keys $ P.untypedPatch p)
    setNext Nothing = return Nothing
    setNext (Just s) = liftPG $ \c -> do
      void $ P.create (mkState s) c
      void $ P.update uid (P.put delayedState Nothing P.empty) c
      return $ Just s

    mkState s = P.put State.eventId evIdt $
                P.put State.userId uid    $
                P.put State.state  s      $
                P.empty

data UserStateEnv = UserStateEnv { lastState :: UserStateVal
                                 , delayed   :: Maybe UserStateVal
                                 , evType    :: EventType
                                 , mdlName   :: Text
                                 , mdlFlds   :: [Text]
                                 }
type UserStateM   = RWS UserStateEnv [Bool] (Maybe UserStateVal) ()

execUserStateEnv :: UserStateEnv -> UserStateM -> Maybe UserStateVal
execUserStateEnv s c = fst $ execRWS c s Nothing

data Matcher = Fields [(Text, Text)] | Models [Text] | NoModel

-- | Calculate next state
nextState :: UserStateVal
          -- ^ Last user state
          -> (Maybe UserStateVal)
          -- ^ Delayed user state
          -> EventType
          -> Text
          -- ^ Model name
          -> [Text]
          -- ^ Field names
          -> Maybe UserStateVal
nextState lastState delayed evt mname fld =
  execUserStateEnv (UserStateEnv lastState delayed evt mname fld) $ do
    change ([Busy] >>> Ready) $
      -- TODO Remove redundant Call.endDate clause here as a call
      -- action is always closed when an associated call is closed
      on Update $ Fields [field Call.endDate, field Action.result]
    change ([Ready] >>> Busy) $ do
      on Update $ Fields [field Action.openTime]
    change ([LoggedOut] >>> Ready)     $ on Login  NoModel
    change (allStates   >>> LoggedOut) $ on Logout NoModel
    case delayed of
      Nothing     -> change ([ServiceBreak] >>> Ready) $
        on Update $ Fields [field delayedState]
      Just Ready  -> change ([Rest, Dinner, ServiceBreak] >>> Ready) $
        on Update $ Fields [field delayedState]
      Just dState -> change ([Ready, Rest, Dinner] >>> dState) $
        on Update $ Fields [field delayedState]

    -- Check if we can switch user into delayed state
    checkDelayed
  where
    field :: forall t n d m1.(Model m1, KnownSymbol n)
          => (m1 -> F t n d) -> (Text, Text)
    field f = (modelName (modelInfo :: ModelInfo m1), fieldName f)
    model :: forall t n d m1.(Model m1, KnownSymbol n) => (m1 -> F t n d) -> Text
    model _ = modelName (modelInfo :: ModelInfo m1)
    allStates = [minBound .. ]
    checkDelayed = do
      UserStateEnv{..} <- ask
      newstate         <- get
      case (delayed, newstate) of
        (Just _, Just Ready) -> put delayed
        _                     -> return ()

on :: EventType -> Matcher -> UserStateM
on tpe matcher = do
  UserStateEnv{..} <- ask
  let applicable = evType == tpe && isMatch mdlName mdlFlds matcher
  tell [applicable]
  where
    isMatch _     _ NoModel         = True
    isMatch mname _ (Models mnames) = mname `elem` mnames
    isMatch mname fld (Fields fs)   =
      not $ null $ fs `intersect` map (mname,) fld

change :: States -> UserStateM -> UserStateM
change (States from to) onFn = do
  lState <- lastState <$> ask
  ons    <- snd <$> listen onFn
  case lState `elem` from && (or ons) of
    False -> return ()
    True  -> put (Just to)

-- Utils -----------------------------------------------------------------------

getRealUid :: AppHandler (Maybe (IdentI Usermeta))
getRealUid = do
  mbu <- withAuth currentUser
  case mbu of
    Just u -> do
      [Only uid] <- query
                [sql| SELECT id from usermetatbl where uid::text = ?|] $
                Only (unUid $ fromJust $ userId $ u)
      return uid
    Nothing -> return Nothing

addIdent :: forall m.Model m => IdentI m -> Patch Event -> Patch Event
addIdent idt p =
  P.put E.modelName mname $ P.put E.modelId (identVal idt) p
  where
    mname = modelName $ (modelInfo :: ModelInfo m)

setUsr :: Maybe (IdentI Usermeta) -> Patch Event -> Patch Event
setUsr usr p = P.put E.userid usr p
