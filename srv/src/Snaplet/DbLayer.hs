
module Snaplet.DbLayer
  (create
  ,read
  ,update
  ,search
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

import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple (pgsInit)
import Snap.Snaplet.RedisDB (redisDBInit)
import qualified Database.Redis as Redis
import qualified Snaplet.DbLayer.RedisCRUD as Redis

import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers



create model commit = do
  commit' <- triggerCreate model commit
  let obj = Map.union commit' commit
  objId <- Redis.create redis model obj
  let fullId = B.concat [model, ":", objId]
  changes <- triggerUpdate fullId obj
  Right _ <- Redis.updateMany redis changes
  return $ Map.insert "id" objId
         $ (changes Map.! fullId) Map.\\ commit


read model objId = do
  res <- Redis.read redis model objId
  -- FIXME: catch NotFound => search in postgres
  return res


update model objId commit = do
  let fullId = B.concat [model, ":", objId]
  -- FIXME: catch NotFound => transfer from postgres to redis
  -- (Copy on write)
  changes <- triggerUpdate fullId commit
  Right _ <- Redis.updateMany redis changes
  return $ (changes Map.! fullId) Map.\\ commit


search ixName val = do
  ix <- gets $ (Map.! ixName) . indices
  ixData <- liftIO $ readTVarIO ix
  let ids = Set.toList $ Map.findWithDefault Set.empty val ixData
  forM ids $ Redis.read' redis

readAll model = Redis.readAll redis model
  
{-
  \i -> do
    obj <- Redis.read' redis i
    return $ map (\f -> Map.findWithDefault "" f obj) fields
-}


initDbLayer :: SnapletInit b (DbLayer b)
initDbLayer = makeSnaplet "db-layer" "Storage abstraction"
  Nothing $ DbLayer
    <$> nestSnaplet "redis" redis
          (redisDBInit Redis.defaultConnectInfo)
    <*> nestSnaplet "pgsql" postgres pgsInit
    <*> liftIO createIndices

----------------------------------------------------------------------

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
