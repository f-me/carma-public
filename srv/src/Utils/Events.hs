{-# LANGUAGE ScopedTypeVariables, QuasiQuotes, FlexibleContexts #-}

module Utils.Events where

import           Prelude hiding (log)

import           Control.Monad
import           Control.Monad.RWS

import           Data.String (fromString)
import           Text.Printf

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Time.Clock (getCurrentTime)
import           GHC.TypeLits

import           Data.Model
import           Data.Model.Patch (Patch)
import qualified Data.Model.Patch     as P
import qualified Data.Model.Patch.Sql as P

import           Snap
import           Snap.Snaplet.Auth
import           Snaplet.Auth.Class
import           Snap.Snaplet.PostgresqlSimple ( HasPostgres(..)
                                               , query
                                               , Only(..)
                                               )
import           Database.PostgreSQL.Simple.SqlQQ

import           Carma.Model
import           Carma.Model.Event (Event, EventType(..))
import qualified Carma.Model.Event     as E
import           Carma.Model.UserState (UserState, UserStateVal(..))
import qualified Carma.Model.UserState as State
import           Carma.Model.Usermeta  (Usermeta(..))
import           Carma.Model.Action    (Action)
import qualified Carma.Model.Action as Action
import qualified Carma.Model.Call   as Call

import           Snaplet.Search.Types
import           Snaplet.Messenger
import           Snaplet.Messenger.Class

import           Util
import           Utils.LegacyModel


-- | Create `Event` for login/logout fact
logLogin :: (HasPostgres (Handler b m), HasAuth b, HasMsg b)
         => EventType -> Handler b m ()
logLogin tpe = do
  uid <- getRealUid
  _   <- log $ addIdent uid $ buildEmpty tpe
  return ()

-- | Interface for events from legacy CRUD
logLegacyCRUD :: (HasPostgres (Handler b b1), HasAuth b, HasMsg b
                 , Model m, SingI n)
              => EventType
              -- ^ event type
              -> ByteString
              -- ^ Legacy object identifier `model:id`
              -> (m -> F t n d)
              -- ^ Changed field
              -> Handler b b1 ()
logLegacyCRUD tpe mdl fld = log $ buildLegacy tpe mdl fld

-- | Create event from patch and change user state when needed
log :: (HasPostgres (Handler b m), HasAuth b, HasMsg b)
    => Patch Event -> Handler b m ()
log p = do
  uid <- getRealUid
  id' <- create $ setUsr uid p
  let p' = P.put E.ident id' p
  s <- checkUserState uid p'
  case s of
    Nothing -> return ()
    Just st -> do
      -- well it's hack of course, current time will be little differene
      -- from real ctime of new state
      time <- liftIO $ getCurrentTime
      withMsg $ sendMessage (T.concat ["Usermeta:", T.pack $ show (identVal uid)])
        (P.put currentState      st   $
         P.put currentStateCTime time $
         P.empty)
  return ()

-- Implementation --------------------------------------------------------------

-- | Build `Path Event`
buildFull :: forall m t n d.(Model m, SingI n)
          => EventType
          -- ^ Type of the event
          -> IdentI m
          -- ^ Identifier of model `m`, emitted event
          -> Maybe (m -> F t n d)
          -- ^ Changed field
          -- -> Maybe (Patch m)
          -- ^ The whole patch
          -- FIXME: can't save patch, because current crud using bad types
          -> Patch Event
buildFull tpe idt f =
  P.put E.modelId   mid   $
  P.put E.modelName mname $
  P.put E.field     fname $
  -- P.put E.patch     patch' $
  buildEmpty tpe
   where
     mname = modelName $ (modelInfo :: ModelInfo m)
     mid   = identVal idt
     fname = return . fieldName =<< f
     -- patch' = Just $ toJSON patch

-- | Build `Patch Event` with just user and type
buildEmpty :: EventType -> Patch Event
buildEmpty tpe = P.put E.eventType tpe $  P.empty

-- | Create `Event` in `postgres` from `Patch Event`, return it's id
create :: HasPostgres m
       => Patch Event
       -> m (IdentI Event)
create ev = withPG $ \c -> liftIO $ P.create ev c

-- | Build log event for legacy model crud
buildLegacy :: forall m t n d.(Model m, SingI n)
            => EventType
            -- ^ Type of the event
            -> ByteString
            -- ^ legacy id `model:id`
            -> (m -> F t n d)
            -- ^ Changed field
            -> Patch Event
buildLegacy tpe objId fld =
  buildFull tpe idt (Just fld)
  where
    idt        = readIdent rawid :: IdentI m
    (_:rawid:_) = B.split ':' objId

data States = States { from :: [UserStateVal], to :: UserStateVal }
(>>>) :: [UserStateVal] -> UserStateVal -> States
(>>>) f t = States f t

-- | Check current state and maybe create new
checkUserState :: HasPostgres (Handler b m)
               => IdentI Usermeta
               -> Patch Event
               -> Handler b m (Maybe UserStateVal)
checkUserState uid ev = do
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
    nextState' s d = join $ dispatch (P.get' ev E.modelName) (nextState'' s d)

    nextState'' :: forall m.Model m =>
                   UserStateVal -> Maybe UserStateVal -> m -> Maybe UserStateVal
    nextState'' s d _ =
      let mname = modelName (modelInfo :: ModelInfo m)
      in nextState s d (P.get' ev E.eventType) mname (join $ P.get ev E.field)

    setNext Nothing = return Nothing
    setNext (Just s) = withPG $ \c -> do
      void $ P.create (mkState s) c
      void $ P.update uid (P.put delayedState Nothing P.empty) c
      return $ Just s

    mkState s = P.put State.eventId (P.get' ev E.ident) $
                P.put State.userId uid                  $
                P.put State.state  s                    $
                P.empty

data UserStateEnv = UserStateEnv { lastState :: UserStateVal
                                 , delayed   :: Maybe UserStateVal
                                 , evType    :: EventType
                                 , mdlName   :: Text
                                 , mdlFld    :: Maybe Text
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
          -> Maybe Text
          -- ^ Field name
          -> Maybe UserStateVal
nextState lastState delayed evt mname fld =
  execUserStateEnv (UserStateEnv lastState delayed evt mname fld) $ do
    change ([Busy] >>> Ready) $
      on Update $ Fields [field Call.endDate, field Action.result]
    change ([Ready] >>> Busy) $ do
      on Create $ Models [model Call.ident]
      on Update $ Fields [field Action.openTime]
    change ([LoggedOut] >>> Ready)     $ on Login  NoModel
    change (allStates   >>> LoggedOut) $ on Logout NoModel
    case delayed of
      Nothing     -> return ()
      Just Ready  -> change ([Rest, Dinner, ServiceBreak] >>> Ready) $
        on Update $ Fields [field delayedState]
      Just dState -> change ([Ready] >>> dState) $
        on Update $ Fields [field delayedState]

    -- Check if we can switch user into delayed state
    checkDelayed
  where
    field :: forall t n d m1.(Model m1, SingI n)
          => (m1 -> F t n d) -> (Text, Text)
    field f = (modelName (modelInfo :: ModelInfo m1), fieldName f)
    model :: forall t n d m1.(Model m1, SingI n) => (m1 -> F t n d) -> Text
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
  let applicable = evType == tpe && isMatch mdlName mdlFld matcher
  tell [applicable]
  where
    isMatch _     _ NoModel         = True
    isMatch mname _ (Models mnames) = mname `elem` mnames
    isMatch mname fld (Fields fs)   =
      maybe False (\f -> (mname,f) `elem` fs) fld

change :: States -> UserStateM -> UserStateM
change (States from to) onFn = do
  lState <- lastState <$> ask
  ons    <- snd <$> listen onFn
  case lState `elem` from && (any id ons) of
    False -> return ()
    True  -> put (Just to)

-- Utils -----------------------------------------------------------------------

getRealUid :: (HasPostgres (Handler b m), HasAuth b)
           => Handler b m (IdentI Usermeta)
getRealUid = do
  Just u <- withAuth currentUser
  [Only uid] <- query
                [sql| SELECT id from usermetatbl where uid::text = ?|] $
                Only (unUid $ fromJust $ userId $ u)
  return uid

mkActionId :: ByteString -> IdentI Action
mkActionId bs = readIdent bs

addIdent :: forall m.Model m => IdentI m -> Patch Event -> Patch Event
addIdent idt p =
  P.put E.modelName mname $ P.put E.modelId (identVal idt) p
  where
    mname = modelName $ (modelInfo :: ModelInfo m)

setUsr :: IdentI Usermeta -> Patch Event -> Patch Event
setUsr usr p = P.put E.userid usr p
