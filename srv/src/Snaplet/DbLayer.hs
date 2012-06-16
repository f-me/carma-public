
module Snaplet.DbLayer where


import Control.Applicative
import Control.Monad.State
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple (pgsInit)
import Snap.Snaplet.RedisDB (redisDBInit)
import qualified Database.Redis as Redis (defaultConnectInfo)
import qualified Snaplet.DbLayer.RedisCRUD as Redis

import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers



create model commit = do
  objId <- Redis.create redis model commit -- create without commit
  -- commit' <- runTriggers createTriggers objId M.empty $ insert id objId commit
  -- commit'' <- runTriggers updateTriggers objId (singleton id ObjId) commit'
  return $ Map.singleton ("id" :: ByteString) objId


read model objId = do
  res <- Redis.read redis model objId
  -- FIXME: catch NotFound => search in postgres
  return res


update model objId commit = do
  let fullId = B.concat [model, ":", objId]
  -- FIXME: catch NotFound => transfer from postgres to redis
  -- (Copy on write)
  changes <- triggerUpdate fullId commit
  Redis.updateMany redis changes
  return $ changes Map.! fullId


initDbLayer :: SnapletInit b (DbLayer b)
initDbLayer = makeSnaplet "db-layer" "Storage abstraction"
  Nothing $ DbLayer
    <$> nestSnaplet "redis" redis
          (redisDBInit Redis.defaultConnectInfo)
    <*> nestSnaplet "pgsql" postgres pgsInit
