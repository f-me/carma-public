
module DbLayer where


import Control.Monad.State
import Data.Functor
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Snap.Snaplet (with)
import Snaplet.SiteConfig
import Snaplet.SiteConfig.Triggers

import Application
import qualified RedisCRUD
import DbTriggers



dbCreate model commit = do
  objId <- RedisCRUD.create redis model commit -- create without commit
  -- commit' <- runTriggers createTriggers objId M.empty $ insert id objId commit
  -- commit'' <- runTriggers updateTriggers objId (singleton id ObjId) commit'
  return $ Map.singleton ("id" :: ByteString) objId


dbRead model objId = do
  res <- RedisCRUD.read redis model objId
  -- FIXME: catch NotFound => search in postgres
  return res


dbUpdate model objId commit = do
  let fullId = B.concat [model, ":", objId]
  -- FIXME: catch NotFound => transfer from postgres to redis
  -- (Copy on write)
  updTriggers <- with siteConfig $ gets (updateTriggers.triggers)
  changes <- runTriggers updTriggers fullId commit
  RedisCRUD.updateMany redis changes
  return $ changes Map.! fullId
