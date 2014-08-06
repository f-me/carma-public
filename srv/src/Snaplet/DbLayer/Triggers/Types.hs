{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Snaplet.DbLayer.Triggers.Types where

import Control.Monad.State
import Control.Monad.CatchIO (MonadCatchIO)
import qualified Data.Text.Encoding as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson (Value, object)

import Snap.Snaplet
import Snap.Snaplet.RedisDB (runRedisDB)
import Snaplet.DbLayer.Types
import Snaplet.Auth.Class
import Snaplet.Messenger.Class
import qualified Snaplet.DbLayer.RedisCRUD as Redis
import qualified Database.Redis as Redis

import Util


data TriggerContext b = TriggerContext
  {dbCache :: ObjectMap
  ,updates :: ObjectMap
  ,current :: ObjectMap
  ,futures :: [DbHandler b ()]
  }

emptyContext :: TriggerContext b
emptyContext = TriggerContext Map.empty Map.empty Map.empty []

newtype TriggerMonad b r = TriggerMonad {
    runTriggerMonad :: StateT (TriggerContext b) (Handler b (DbLayer b)) r
    } deriving (Functor, Monad, MonadIO, MonadCatchIO, MonadState (TriggerContext b))

type Trigger b = ObjectId -> FieldValue -> TriggerMonad b ()
type TriggerMap b = Map ModelName (Map FieldName [Trigger b])

class ( Functor (m b)
      , MonadCatchIO (m b)
      , MonadState (TriggerContext b) (m b)
      , HasAuth b
      , HasMsg  b
      )
      => MonadTrigger m b where
    createObject :: ModelName -> Object -> m b ObjectId
    readObject :: ObjectId -> m b Object
    redisDel :: [ObjectId] -> m b (Either Redis.Reply Integer)
    dateNow :: (Int -> Int) -> m b FieldValue
    liftDb :: Handler b (DbLayer b) r -> m b r

logObject :: MonadIO m => String -> Value -> m ()
logObject name v
  = syslogJSON Debug "trigger/object" ["trigger" .= name, "data" .= v]

reply :: Either Redis.Reply a -> Either String a
reply (Left r) = Left (show r)
reply (Right r) = Right r

instance (HasAuth b, HasMsg b) => MonadTrigger TriggerMonad b where
    createObject model obj = logExceptions "trigger/create" $ liftDb $ do
        i <- Redis.create redis model obj
        logObject "create" $ object [
            "model" .= model,
            "object" .= obj,
            "id" .= i]
        return i
    readObject key = do
        v <- TriggerMonad $ lift $ Redis.read' redis key
        liftDb $ logObject "trigger/read" $ object [
            "key" .= key,
            "result" .= v]
        return v
    redisDel keys = logExceptions "trigger/del" $ liftDb $ do
        logObject "del" $ object ["keys" .= keys]
        runRedisDB redis $ Redis.del $ map T.encodeUtf8 keys
    dateNow fn = TriggerMonad $ liftIO $ projNow fn
    liftDb act = TriggerMonad $ lift act
