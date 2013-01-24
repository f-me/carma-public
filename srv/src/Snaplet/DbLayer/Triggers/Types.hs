{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, RankNTypes, MultiParamTypeClasses #-}

module Snaplet.DbLayer.Triggers.Types where

import Prelude hiding (log)

import Control.Concurrent (myThreadId)
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Aeson (Value, encode, object, (.=), toJSON)
import qualified Data.Aeson as A (Value(..), Object)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Snap.Snaplet
import Snap.Snaplet.RedisDB (runRedisDB)
import Snaplet.DbLayer.Types
import qualified Snaplet.DbLayer.RedisCRUD as Redis
import qualified Database.Redis as Redis

import System.Log

data TriggerContext = TriggerContext
  {dbCache :: ObjectMap
  ,updates :: ObjectMap
  ,current :: ObjectMap
  }

emptyContext :: TriggerContext
emptyContext = TriggerContext Map.empty Map.empty Map.empty

newtype TriggerMonad b r = TriggerMonad {
    runTriggerMonad :: StateT TriggerContext (Handler b (DbLayer b)) r }
        deriving (Functor, Monad, MonadIO, MonadState TriggerContext)
type Trigger b = ObjectId -> FieldValue -> TriggerMonad b ()
type TriggerMap b = Map ModelName (Map FieldName [Trigger b])

class (Functor (m b), MonadIO (m b), MonadState TriggerContext (m b)) => MonadTrigger m b where
    createObject :: ModelName -> Object -> m b ByteString
    readObject :: ByteString -> m b (Map.Map ByteString ByteString)
    redisLPush :: ByteString -> [ByteString] -> m b (Either Redis.Reply Integer)
    redisHGet :: ByteString -> ByteString -> m b (Either Redis.Reply (Maybe ByteString))
    redisHGetAll :: ByteString -> m b (Either Redis.Reply [(ByteString, ByteString)])
    redisDel :: [ByteString] -> m b (Either Redis.Reply Integer)
    dateNow :: (Int -> Int) -> m b FieldValue
    liftDb :: Handler b (DbLayer b) r -> m b r

logObject :: MonadLog m => Value -> m ()
logObject (A.Object v) = do
    thId <- liftIO myThreadId
    log Trace . T.decodeUtf8 . toStrict . encode
        . A.Object . HM.insert "threadId" (toJSON $ show thId) $ v

reply :: Either Redis.Reply a -> Either String a
reply (Left r) = Left (show r)
reply (Right r) = Right r

instance MonadTrigger TriggerMonad b where
    createObject model obj = liftDb $ scope "trigger" $ scope "create" $ do
        i <- Redis.create redis model obj
        logObject $ object [
            "model" .= model,
            "object" .= obj,
            "id" .= i]
        return i
    readObject key = do
        v <- TriggerMonad $ lift $ Redis.read' redis key
        liftDb $ scope "trigger" $ scope "read" $ logObject $ object [
            "key" .= key,
            "result" .= v]
        return v
    redisLPush lst vals = liftDb $ scope "trigger" $ scope "lpush" $ do
        logObject $ object [
            "list" .= lst,
            "values" .= vals]
        runRedisDB redis $ Redis.lpush lst vals
    redisHGet key val = liftDb $ scope "trigger" $ scope "hget" $ do
        result <- runRedisDB redis $ Redis.hget key val
        logObject $ object [
            "key" .= key,
            "member" .= val,
            "result" .= reply result]
        return result
    redisHGetAll key = liftDb $ scope "trigger" $ scope "hgetall" $ do
        result <- runRedisDB redis $ Redis.hgetall key
        logObject $ object [
            "key" .= key,
            "result" .= reply result]
        return result
    redisDel keys = liftDb $ scope "trigger" $ scope "del" $ do
        logObject $ object [
            "keys" .= keys]
        runRedisDB redis $ Redis.del keys
    dateNow fn = TriggerMonad $ liftIO $ fmap (B.pack . show . fn . round . utcTimeToPOSIXSeconds) getCurrentTime
    liftDb act = TriggerMonad $ lift act
