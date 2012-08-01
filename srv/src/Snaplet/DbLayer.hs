module Snaplet.DbLayer
  (create
  ,read
  ,update
  ,delete
  ,search
  ,sync
  ,generateReport
  ,readAll
  ,initDbLayer
  ) where

import Prelude hiding (read, log)
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.String
import qualified Data.Text as T

import Network.URI (parseURI, URI(..))
import qualified Fdds as Fdds
import Data.Configurator

import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple (pgsInit)
import Snap.Snaplet.RedisDB (redisDBInit, runRedisDB)
import Snap.Snaplet.SimpleLog
import qualified Database.Redis as Redis
import qualified Snaplet.DbLayer.RedisCRUD as Redis
import qualified Snaplet.DbLayer.PostgresCRUD as Postgres
import qualified Database.PostgreSQL.Syncs as S
import qualified Database.PostgreSQL.Models as SM

import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers
import Util



create model commit = scoper "create" $ do
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
  Postgres.insert Postgres.modelModels model obj'
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


read model objId = do
  res <- Redis.read redis model objId
  -- FIXME: catch NotFound => search in postgres
  return res


update model objId commit = scoper "update" $ do
  log Trace $ fromString $ "Model: " ++ show model
  --
  let fullId = B.concat [model, ":", objId]
  -- FIXME: catch NotFound => transfer from postgres to redis
  -- (Copy on write)
  changes <- triggerUpdate fullId commit
  Right _ <- Redis.updateMany redis changes
  -- 
  let changes' = Map.mapKeys (B.drop (B.length model + 1)) changes
  log Trace $ fromString $ "Changes: " ++ show changes'
  Postgres.updateMany Postgres.modelModels model changes'
  --
  return $ (changes Map.! fullId) Map.\\ commit

delete model objId = do
  Redis.delete redis model objId

search ixName val = do
  ix <- gets $ (Map.! ixName) . indices
  ixData <- liftIO $ readTVarIO ix
  let ids = Set.toList $ Map.findWithDefault Set.empty val ixData
  forM ids $ Redis.read' redis

sync :: Handler b (DbLayer b) ()
sync = scope "sync" $ mapM_ syncModel modelList where
  syncModel model = scope "syncModel" $ do
    Right (Just cnt) <- runRedisDB redis $ Redis.get (Redis.modelIdKey model)
    log Trace $ fromString $ "Count of entries for model " ++ show model ++ " is: " ++ show cnt
    case C8.readInt cnt of
      Just (maxId, _) -> forM_ [1..maxId] $ \i -> do
        rec <- Redis.read redis model (C8.pack . show $ i)
        when (not $ Map.null rec) $ void $ Postgres.insertUpdate Postgres.modelModels model (C8.pack . show $ i) rec
      Nothing -> error $ "Invalid id for model " ++ C8.unpack model

  modelList = map C8.pack $ Map.keys (SM.modelsModels Postgres.modelModels)

generateReport :: [T.Text] -> FilePath -> FilePath -> Handler b (DbLayer b) ()
generateReport conds template filename = Postgres.generateReport Postgres.modelModels conds template filename

readAll model = Redis.readAll redis model

-- log politics
logConfig = []

initDbLayer :: SnapletInit b (DbLayer b)
initDbLayer = makeSnaplet "db-layer" "Storage abstraction"
  Nothing $ do
    l <- liftIO $ newLog defaultPolitics logConfig [logger text (file "log/db.log")]
    liftIO $ Postgres.createIO Postgres.modelModels l
    cfg <- getSnapletUserConfig
    DbLayer
      <$> nestSnaplet "redis" redis
            (redisDBInit Redis.defaultConnectInfo)
      <*> nestSnaplet "pgsql" postgres pgsInit
      <*> nestSnaplet "dblog" dbLog (simpleLogInit_ l)
      <*> liftIO triggersConfig
      <*> liftIO createIndices
      <*> (liftIO $ fddsConfig cfg)

----------------------------------------------------------------------
triggersConfig = do
  recs <- readJSON "resources/site-config/recommendations.json"
  return $ TriggersConfig recs

createIndices = return Map.empty

fddsConfig cfg = do
  uri   <- require cfg "fdds-uri"
  login <- require cfg "fdds-login"
  passw <- require cfg "fdds-password"
  return $ Fdds.Conf (fromJust $ parseURI uri) login passw
