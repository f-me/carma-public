{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Snaplet.DbLayer
  (create
  ,read
  ,read'
  ,selectDb
  ,update
  ,delete
  ,exists
  ,submitTask
  ,generateReport
  ,readAll
  ,smsProcessing
  ,initDbLayer
  ,findOrCreate
  ) where

import Prelude hiding (read)
import Control.Applicative
import Control.Lens (Lens')
import Control.Monad.State
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromJust, isJust)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

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
import qualified Database.PostgreSQL.Sync.Base as S

import Snaplet.DbLayer.Types
import qualified Carma.ModelTables as MT (loadTables)
import Snaplet.DbLayer.Triggers
import Snaplet.DbLayer.Dictionary (readRKCCalc)
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
       -> Handler b (DbLayer b) (Map.Map FieldName ByteString)
create model commit = do
  tbls <- gets syncTables
  syslogJSON Debug "DbLayer/create" ["model" .= model, "commit" .= commit]
  --
  obj <- triggerCreate model =<< applyDefaults model commit
  objId <- Redis.create redis model obj
  --
  let obj' = Map.insert (C8.pack "id") objId obj
  syslogJSON Debug "DbLayer/create" ["obj'" .= obj']
  --
  Postgres.insert tbls model obj'
{-
  let fullId = B.concat [model, ":", objId]
  changes <- triggerUpdate fullId obj
  --
  let changes' = Map.mapKeys (B.drop (B.length model + 1)) changes
  log Trace $ fromString $ "Changes: " ++ show changes'
  --
  Right _ <- Redis.updateMany redis changes
-}

  when (model == "call") $
    Evt.logLegacyCRUD Create (B.concat [model, ":", objId]) Call.ident

  let result = Map.insert "id" objId $ obj Map.\\ commit
  syslogJSON Debug "DbLayer/create/result" ["result" .= result]
  return result

findOrCreate :: (HasAuth b, HasMsg b)
             => ModelName -> ObjectId -> Object
             -> Handler b (DbLayer b) (Map.Map ByteString ByteString)
findOrCreate model objId commit = do
  r <- read model objId
  case Map.toList r of
    [] -> do
      obj <- triggerCreate model =<< applyDefaults model commit
      Redis.create' redis model objId obj
    _  -> return r

read :: ModelName -> ObjectId
     -> Handler b (DbLayer b) (Map.Map ByteString ByteString)
read model objId = do
  res <- Redis.read redis model objId
  -- FIXME: catch NotFound => search in postgres
  return res

read' :: ByteString -> Handler b (DbLayer b) (Map.Map ByteString ByteString)
read' objId = Redis.read' redis objId

update :: (HasAuth b, HasMsg b)
       =>  ModelName -> ObjectId -> Object
       -> Handler b (DbLayer b) (Map.Map FieldName ByteString)
update model objId commit = do
  tbls <- gets syncTables
  syslogJSON Debug "DbLayer/update" ["model" .= model, "commit" .= commit]
  --
  let fullId = B.concat [model, ":", objId]
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
                 Map.mapKeys (toPair . C8.split ':')              $
                 changes
  syslogJSON Debug "DbLayer/update" ["changes" .= changes]
  Postgres.insertUpdateMany tbls changes'
  sequence_ futures -- run delayed actions

  -- mapM_ (uncurry $ Evt.logLegacyCRUD Update) $ Map.toList changes'
  --
  let stripUnchanged orig = Map.filterWithKey (\k v -> Map.lookup k orig /= Just v)
      commit' = stripUnchanged commit $ changes Map.! fullId

  withMsg $ sendMessage (T.decodeUtf8 fullId) commit'
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


generateReport :: (T.Text -> [T.Text]) -> FilePath -> FilePath -> Handler b (DbLayer b) ()
generateReport superCond template filename = do
    rels <- gets syncRelations
    tbls <- gets syncTables
    Postgres.generateReport tbls (S.relationsConditions rels) superCond template filename

readAll :: ByteString -> Handler b (DbLayer b) [Map.Map ByteString ByteString]
readAll model = Redis.readAll redis model

smsProcessing :: Handler b (DbLayer b) Integer
smsProcessing = runRedisDB redis $ do
  (Right i) <- Redis.llen "smspost"
  (Right ri) <- Redis.llen "smspost:retry"
  return $ i + ri

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
      <*> (liftIO $ readRKCCalc cfgDir)
