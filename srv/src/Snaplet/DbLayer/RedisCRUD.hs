
module Snaplet.DbLayer.RedisCRUD where

import Prelude hiding (read)
import Control.Monad.IO.Class
import Control.Monad (forM)
import Data.Functor
import Data.Either
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Database.Redis
import Snap.Snaplet.RedisDB


type ModelName = B.ByteString

objKey :: ModelName -> B.ByteString -> B.ByteString
objKey model objId = B.concat [model, ":", objId]

modelIdKey :: ModelName -> B.ByteString
modelIdKey model = B.concat ["global:", model, ":id"]


read r model objId = runRedisDB r $ do
  let key = objKey model objId
  Right res  <- fmap M.fromList <$> hgetall key
  return res

read' r objId = runRedisDB r $ do
  Right res  <- fmap M.fromList <$> hgetall objId
  return res

readAll r model = runRedisDB r $ do
  Right ids <- keys $ B.concat [model, ":*"]
  res <- forM ids $ \i ->
    fmap (M.insert "id" i . M.fromList) <$> hgetall i
  return $ rights res

create r model commit = runRedisDB r $ do
  Right n <- incr $ modelIdKey model
  let idStr   = B.pack $ show n
  let key     = objKey model idStr
  let commit' = M.insert "id" idStr commit
  Right _ <- hmset key $ M.toList commit'
  return idStr


update r model objId commit = runRedisDB r $ do
  let key = objKey model objId
  Right _ <- hmset key $ M.toList commit
  return ()


updateMany r objectMap = runRedisDB r $ do
  res <- forM (M.toList objectMap) $ \(k,obj) ->
        if M.null obj then return $ Right undefined
                      else hmset k $ M.toList obj
  case lefts res of
    [] -> return $ Right ()
    _  -> error "updateMany failed"

delete r model objId = runRedisDB r $ do
  let key = objKey model objId
  Right _ <- del [key]
  return ()
