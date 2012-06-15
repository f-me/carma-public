{-# LANGUAGE TemplateHaskell #-}
module Snaplet.DbLayer where


import Control.Applicative
import Control.Monad.State
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Lens.Template

import Snap.Snaplet
import Snaplet.SiteConfig
import Snaplet.SiteConfig.Triggers
import Snap.Snaplet.PostgresqlSimple (pgsInit, Postgres)

import Snap.Snaplet.RedisDB (RedisDB, redisDBInit)
import qualified Database.Redis as Redis (defaultConnectInfo)
import qualified Snaplet.DbLayer.RedisCRUD as Redis
import Snaplet.DbLayer.Triggers


data DbLayer b = DbLayer
    {_siteCfg    :: Snaplet (SiteConfig b)
    ,_redis      :: Snaplet RedisDB
    ,_postgres   :: Snaplet Postgres
    }

makeLens ''DbLayer



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
  updTriggers <- with siteCfg $ gets (updateTriggers.triggers)
  changes <- runTriggers updTriggers fullId commit
  Redis.updateMany redis changes
  return $ changes Map.! fullId


initDbLayer :: Snaplet (SiteConfig b) -> SnapletInit b (DbLayer b)
initDbLayer cfg = makeSnaplet "db-layer" "Storage abstraction"
  Nothing $ DbLayer
    <$> pure cfg
    <*> nestSnaplet "redis" redis
          (redisDBInit Redis.defaultConnectInfo)
    <*> nestSnaplet "pgsql" postgres pgsInit
