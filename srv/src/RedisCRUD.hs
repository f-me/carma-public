
module RedisCRUD where

import Prelude hiding (read)
import Data.Functor
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
