{-# LANGUAGE CPP #-}

module Snaplet.DbLayer
  (create
  ,read
  ,read'
  ,update
  ,delete
  ,submitTask
  ,searchFullText
  ,generateReport
  ,readAll
  ,smsProcessing
  ,initDbLayer
  ,findOrCreate
  ) where

import Prelude hiding (read, log)
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent.STM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromJust, isJust)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.URI (parseURI)
import qualified Fdds
import Data.Configurator

import WeatherApi.WWOnline (initApi)

import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple (pgsInit)
import Snap.Snaplet.RedisDB (redisDBInit, runRedisDB)
import Snap.Snaplet.SimpleLog
#if !defined(mingw32_HOST_OS)
import System.Log.Syslog
#endif
import qualified Database.Redis as Redis
import qualified Snaplet.DbLayer.RedisCRUD as Redis
import qualified Snaplet.DbLayer.PostgresCRUD as Postgres
import qualified Database.PostgreSQL.Sync.Base as S
import qualified Database.PostgreSQL.Sync.Types as S

import Snaplet.DbLayer.Types
import qualified Carma.ModelTables as MT (loadTables)
import Snaplet.DbLayer.Triggers
import Snaplet.DbLayer.Dictionary (readRKCCalc)
import DictionaryCache
import Util
import RuntimeFlag

create :: ModelName -> Object -> Handler b (DbLayer b) (Map.Map FieldName ByteString)
create model commit = scoper "create" $ do
  tbls <- gets syncTables
  log Trace $ fromString $ "Model: " ++ show model
  log Trace $ fromString $ "Commit: " ++ show commit
  --
  commit' <- triggerCreate model commit
  let obj = Map.union commit' commit
  objId <- Redis.create redis model obj
  --
  let obj' = Map.insert (C8.pack "id") objId obj
  log Trace $ fromString $ "Object with id: " ++ show obj'
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
  return $ Map.insert "id" objId
         $ obj Map.\\ commit

findOrCreate :: ByteString -> ByteString -> Object -> Handler b (DbLayer b) (Map.Map ByteString ByteString)
findOrCreate model objId commit = do
  r <- read model objId
  case Map.toList r of
    [] -> do
      commit' <- triggerCreate model commit
      let obj = Map.union commit' commit
      Redis.create' redis model objId obj
    _  -> return r

read :: ByteString -> ByteString -> Handler b (DbLayer b) (Map.Map ByteString ByteString)
read model objId = do
  res <- Redis.read redis model objId
  -- FIXME: catch NotFound => search in postgres
  return res

read' :: ByteString -> Handler b (DbLayer b) (Map.Map ByteString ByteString)
read' objId = Redis.read' redis objId

update :: ByteString -> ByteString -> Object -> Handler b (DbLayer b) (Map.Map FieldName ByteString)
update model objId commit = scoper "update" $ do
  tbls <- gets syncTables
  log Trace $ fromString $ "Model: " ++ show model
  --
  let fullId = B.concat [model, ":", objId]
  -- FIXME: catch NotFound => transfer from postgres to redis
  -- (Copy on write)
  changes <- triggerUpdate fullId commit
  Right _ <- Redis.updateMany redis changes
  -- 
  let
    toPair [x, y] = Just (x, y)
    toPair _ = Nothing

  let changes' = Map.mapWithKey (\(_,k) v -> Map.insert "id" k v) . Map.mapKeys fromJust . Map.filterWithKey (\k v -> isJust k) . Map.mapKeys (toPair . C8.split ':') $ changes
  log Trace $ fromString $ "Changes: " ++ show changes'
  Postgres.insertUpdateMany tbls changes'
  --
  let stripUnchanged orig = Map.filterWithKey (\k v -> Map.lookup k orig /= Just v)
  return $ stripUnchanged commit $ changes Map.! fullId

delete :: ByteString -> ByteString -> Handler b (DbLayer b) ()
delete model objId = Redis.delete redis model objId


submitTask :: ByteString -> ByteString -> Handler b (DbLayer b) (Either Redis.Reply Integer)
submitTask queueName taskId
  = runRedisDB redis
  $ Redis.lpush queueName [taskId]

searchFullText :: ByteString -> [ByteString] -> [ByteString] -> ByteString -> Int -> Handler b (DbLayer b) [[ByteString]]
searchFullText mname fs sels q lim = do
  tbls <- gets syncTables
  res <- Postgres.search tbls mname fs sels q lim
  return $ map (map showValue) res
  where
    showValue :: S.FieldValue -> ByteString
    showValue (S.IntValue s) = T.encodeUtf8 . T.pack . show $ s
    showValue (S.DoubleValue s) = T.encodeUtf8 . T.pack . show $ s
    showValue (S.BoolValue s) = T.encodeUtf8 . T.pack . show $ s
    showValue (S.StringValue s) = T.encodeUtf8 . T.pack $ s
    showValue (S.TimeValue s) = T.encodeUtf8 . T.pack . (show :: Integer -> String) . floor $ s
    showValue _ = C8.empty

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


initDbLayer
  :: UsersDict -> TVar RuntimeFlags -> FilePath
  -> SnapletInit b (DbLayer b)
initDbLayer allU rtF cfgDir = makeSnaplet "db-layer" "Storage abstraction"
  Nothing $ do
    l <- liftIO $ newLog (fileCfg "resources/site-config/db-log.cfg" 10)
#if !defined(mingw32_HOST_OS)
      [logger text (file "log/db.log"), syslog "carma" [PID] USER]
#else
      [logger text (file "log/db.log")]
#endif
    liftIO $ withLog l $ log Info "Server started"
    rels <- liftIO $ Postgres.loadRelations "resources/site-config/syncs.json" l
    tbls <- liftIO $ MT.loadTables "resources/site-config/models" "resources/site-config/field-groups.json"
    liftIO $ Postgres.createIO tbls l
    cfg <- getSnapletUserConfig
    wkey <- liftIO $ lookupDefault "" cfg "weather-key"

    let usrDic
          = Map.fromList
            [(u' Map.! "value", u' Map.! "label")
            | u <- us
            , let u' = Map.map T.decodeUtf8 u]
          where UsersDict us = allU

    dc <- liftIO
          $ loadDictionaries usrDic "resources/site-config/dictionaries"
          >>= newTVarIO

    DbLayer
      <$> nestSnaplet "redis" redis
            (redisDBInit Redis.defaultConnectInfo)
      <*> nestSnaplet "pgsql" postgres pgsInit
      <*> nestSnaplet "dblog" dbLog (simpleLogInit_ l)
      <*> liftIO triggersConfig
      <*> (liftIO $ fddsConfig cfg)
      <*> (return rels)
      <*> (return tbls)
      <*> (return allU)
      <*> (return dc)
      <*> (return $ initApi wkey)
      <*> (liftIO $ readRKCCalc cfgDir)
      <*> pure rtF
----------------------------------------------------------------------
triggersConfig :: IO TriggersConfig
triggersConfig = do
  recs <- readJSON "resources/site-config/recommendations.json"
  return $ TriggersConfig recs


fddsConfig cfg = do
  uri   <- require cfg "fdds-uri"
  login <- require cfg "fdds-login"
  passw <- require cfg "fdds-password"
  return $ Fdds.Conf (fromJust $ parseURI uri) login passw
