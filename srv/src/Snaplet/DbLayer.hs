{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Snaplet.DbLayer
  (create
  ,read
  ,read'
  ,selectDb
  ,update
  ,delete
  ,exists
  ,submitTask
  ,readAll
  ,initDbLayer
  ,findOrCreate
  ) where

import Prelude hiding (read)
import Control.Applicative
import Control.Lens (Lens')
import Control.Monad.State
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.ByteString (ByteString)
import qualified Data.Text          as T

import Data.Configurator

import WeatherApi.WWOnline (initApi)

import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple (Postgres, pgsInit)
import Snap.Snaplet.RedisDB (redisDBInitConf, runRedisDB)

import qualified Database.Redis as Redis hiding (exists)

import qualified Snaplet.DbLayer.RedisCRUD as Redis
import qualified Snaplet.DbLayer.PostgresCRUD as Postgres
import Snaplet.DbLayer.Util (selectDb)

import Snaplet.DbLayer.Types
import qualified Carma.ModelTables as MT (loadTables)
import Snaplet.DbLayer.Triggers
import Snaplet.Auth.Class
import Snaplet.Messenger (sendMessage)
import Snaplet.Messenger.Class
import DictionaryCache

import qualified Carma.Model.Call as Call
import           Carma.Model.Event (EventType(..))
import qualified Utils.Events as Evt
import Util


create :: (HasAuth b, HasMsg b)
       => ModelName -> Object
       -> Handler b (DbLayer b) Object
create model commit = do
  tbls <- gets syncTables
  syslogJSON Debug "DbLayer/create" ["model" .= model, "commit" .= commit]
  --
  obj <- triggerCreate model =<< applyDefaults model commit
  objId <- Redis.create redis model obj
  --
  let obj' = Map.insert "id" objId obj
  syslogJSON Debug "DbLayer/create" ["obj'" .= obj']
  --
  Postgres.insert tbls model obj'

  when (model == "call") $
    Evt.logLegacyCRUD Create (T.concat [model, ":", objId]) Call.ident

  let result = Map.insert "id" objId $ obj Map.\\ commit
  syslogJSON Debug "DbLayer/create/result" ["result" .= result]
  return result

findOrCreate :: (HasAuth b, HasMsg b)
             => ModelName -> ObjectId -> Object
             -> Handler b (DbLayer b) Object
findOrCreate model objId commit = do
  r <- read model objId
  case Map.toList r of
    [] -> do
      obj <- triggerCreate model =<< applyDefaults model commit
      Redis.create' redis model objId obj
    _  -> return r

read :: ModelName -> ObjectId
     -> Handler b (DbLayer b) Object
read model objId = do
  res <- Redis.read redis model objId
  -- FIXME: catch NotFound => search in postgres
  return res

read' :: ObjectId -> Handler b (DbLayer b) Object
read' = Redis.read' redis

update :: (HasAuth b, HasMsg b)
       =>  ModelName -> ObjectId -> Object
       -> Handler b (DbLayer b) Object
update model objId commit = do
  tbls <- gets syncTables
  syslogJSON Debug "DbLayer/update" ["model" .= model, "commit" .= commit]
  --
  let fullId = T.concat [model, ":", objId]
  -- FIXME: catch NotFound => transfer from postgres to redis
  -- (Copy on write)
  (futures, changes) <- triggerUpdate model objId commit
  Right _ <- Redis.updateMany redis changes
  --
  let
    toPair [x, y] = Just (x, y)
    toPair _ = Nothing

  let changes' = Map.mapWithKey (\(_,k) v -> Map.insert "id" k v) .
                 Map.mapKeys fromJust                             .
                 Map.filterWithKey (\k _ -> isJust k)             .
                 Map.mapKeys (toPair . T.splitOn ":")             $
                 changes
  syslogJSON Debug "DbLayer/update" ["changes" .= changes]
  Postgres.insertUpdateMany tbls changes'
  sequence_ futures -- run delayed actions

  -- mapM_ (uncurry $ Evt.logLegacyCRUD Update) $ Map.toList changes'
  --
  let stripUnchanged orig = Map.filterWithKey (\k v -> Map.lookup k orig /= Just v)
      commit' = stripUnchanged commit $ changes Map.! fullId

  withMsg $ sendMessage fullId commit'
  syslogJSON Debug "DbLayer/update/result" ["result" .= commit']
  return commit'

delete :: ModelName -> ObjectId -> Handler b (DbLayer b) ()
delete model objId = Redis.delete redis model objId


exists :: ModelName -> ObjectId -> Handler b (DbLayer b) Bool
exists model objId = Redis.exists redis model objId


submitTask :: ByteString -> ByteString -> Handler b (DbLayer b) (Either Redis.Reply Integer)
submitTask queueName taskId
  = runRedisDB redis
  $ Redis.lpush queueName [taskId]


readAll :: ModelName -> Handler b (DbLayer b) [Object]
readAll = Redis.readAll redis


-- TODO Use lens to an external AuthManager
initDbLayer :: Snaplet (AuthManager b)
            -> Lens' b (Snaplet Postgres)
            -- ^ Lens to a snaplet with Postgres DB used for user
            -- authorization.
            -> FilePath
            -> SnapletInit b (DbLayer b)
initDbLayer sessionMgr adb cfgDir = makeSnaplet "db-layer" "Storage abstraction"
  Nothing $ do
    -- syslog Info "Server started"
    rels <- liftIO $ Postgres.loadRelations "resources/site-config/syncs.json"
    tbls <- liftIO $ MT.loadTables "resources/site-config/models" "resources/site-config/field-groups.json"
    cfg <- getSnapletUserConfig
    wkey <- liftIO $ lookupDefault "" cfg "weather-key"

    dc <- liftIO
          $ loadDictionaries "resources/site-config/dictionaries"
          >>= newTVarIO

    DbLayer adb
      <$> nestSnaplet "redis" redis redisDBInitConf
      <*> nestSnaplet "pgsql" postgres pgsInit
      <*> pure sessionMgr
      <*> (return rels)
      <*> (return tbls)
      <*> (return dc)
      <*> (return $ initApi wkey)
