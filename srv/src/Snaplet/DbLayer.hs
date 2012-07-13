
module Snaplet.DbLayer
  (create
  ,read
  ,update
  ,search
  ,sync
  ,readAll
  ,initDbLayer
  ) where

import Prelude hiding (read)
import Control.Applicative
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

import Network.URI (parseURI, URI(..))
import qualified Fdds as Fdds
import Data.Configurator

import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple (pgsInit)
import Snap.Snaplet.RedisDB (redisDBInit, runRedisDB)
import qualified Database.Redis as Redis
import qualified Snaplet.DbLayer.RedisCRUD as Redis
import qualified Snaplet.DbLayer.PostgresCRUD as Postgres
import qualified Database.PostgreSQL.Syncs as S

import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers
import Util



create model commit = do
  liftIO $ putStrLn "CREATE"
  liftIO $ putStrLn $ "  MODEL: " ++ show model
  liftIO $ putStrLn $ "  COMMIT: " ++ show commit
  --
  commit' <- triggerCreate model commit
  let obj = Map.union commit' commit
  objId <- Redis.create redis model obj
  --
  let obj' = Map.insert (C8.pack "id") objId obj
  liftIO $ putStrLn $ "  WITHID: " ++ show obj'
  --
  Postgres.insert Postgres.models model obj'
  
  let fullId = B.concat [model, ":", objId]
  changes <- triggerUpdate fullId obj
  --
  let changes' = Map.mapKeys (B.drop (B.length model + 1)) changes
  liftIO $ putStrLn $ "  CHANGES: " ++ show changes'
  --
  Right _ <- Redis.updateMany redis changes'
  return $ Map.insert "id" objId
         $ (changes Map.! fullId) Map.\\ commit


read model objId = do
  res <- Redis.read redis model objId
  -- FIXME: catch NotFound => search in postgres
  return res


update model objId commit = do
  liftIO $ putStrLn "UPDATE"
  liftIO $ putStrLn $ "  MODEL: " ++ show model
  --
  let fullId = B.concat [model, ":", objId]
  -- FIXME: catch NotFound => transfer from postgres to redis
  -- (Copy on write)
  changes <- triggerUpdate fullId commit
  Right _ <- Redis.updateMany redis changes
  -- 
  let changes' = Map.mapKeys (B.drop (B.length model + 1)) changes
  liftIO $ putStrLn $ "  CHANGES: " ++ show changes'
  Postgres.updateMany Postgres.models model changes'
  --
  return $ (changes Map.! fullId) Map.\\ commit


search ixName val = do
  ix <- gets $ (Map.! ixName) . indices
  ixData <- liftIO $ readTVarIO ix
  let ids = Set.toList $ Map.findWithDefault Set.empty val ixData
  forM ids $ Redis.read' redis

sync :: Handler b (DbLayer b) ()
sync = mapM_ syncModel syncsList where
  syncModel model = do
    Right (Just cnt) <- runRedisDB redis $ Redis.get (Redis.modelIdKey model)
    case C8.readInt cnt of
      Just (maxId, _) -> forM_ [1..maxId] $ \i -> do
        rec <- Redis.read redis model (C8.pack . show $ i)
        Postgres.insertUpdate Postgres.models model (C8.pack . show $ i) rec
      Nothing -> error $ "Invalid id for model " ++ C8.unpack model

  syncsList = map (C8.pack) $ Map.keys (S.syncsSyncs Postgres.models)

readAll model = Redis.readAll redis model
  
{-
  \i -> do
    obj <- Redis.read' redis i
    return $ map (\f -> Map.findWithDefault "" f obj) fields
-}


initDbLayer :: SnapletInit b (DbLayer b)
initDbLayer = makeSnaplet "db-layer" "Storage abstraction"
  Nothing $ do
    liftIO $ Postgres.createIO Postgres.models
    cfg <- getSnapletUserConfig
    DbLayer
      <$> nestSnaplet "redis" redis
            (redisDBInit Redis.defaultConnectInfo)
      <*> nestSnaplet "pgsql" postgres pgsInit
      <*> liftIO triggersConfig
      <*> liftIO createIndices
      <*> (liftIO $ fddsConfig cfg)

----------------------------------------------------------------------
triggersConfig = do
  recs <- readJSON "resources/site-config/recommendations.json"
  return $ TriggersConfig recs

createIndices = return Map.empty
{-
  r <- Redis.connect Redis.defaultConnectInfo
  (actionByGroup, actionByAssignee) <- Redis.runRedis r $ do
    Right actions <- Redis.keys "action:*"
    let mkIx (m1, m2) a = do
          Right res <- Redis.hmget a ["closed", "targetGroup", "assignedTo"]
          return $ case res of
            [Just "false", Just g, Just ""]
              -> (m1, Map.insertWith' Set.union g (Set.singleton a) m2)
            [Just "false", Just g, Just  u]
              -> (Map.insertWith' Set.union u (Set.singleton a) m1, m2)
            _ -> (m1, m2)
    foldM mkIx (Map.empty, Map.empty) actions
  actionByGroup_tvar <- newTVarIO actionByGroup
  actionByAssignee_tvar <- newTVarIO actionByAssignee
  return $ Map.fromList
    [("actionByGroup", actionByGroup_tvar)
    ,("actionByAssignee", actionByAssignee_tvar)
    ]
-}

fddsConfig cfg = do
  uri   <- require cfg "fdds-uri"
  login <- require cfg "fdds-login"
  passw <- require cfg "fdds-password"
  return $ Fdds.Conf (fromJust $ parseURI uri) login passw
