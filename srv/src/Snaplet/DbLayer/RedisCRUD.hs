{-# LANGUAGE Rank2Types #-}
module Snaplet.DbLayer.RedisCRUD where

import Prelude hiding (read)
import Control.Lens
import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.Functor
import Data.Either
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Database.Redis
import Snap.Snaplet
import Snap.Snaplet.RedisDB
import Snaplet.DbLayer.Types


objKey :: ModelName -> B.ByteString -> B.ByteString
objKey model objId = B.concat [model, ":", objId]

modelIdKey :: ModelName -> B.ByteString
modelIdKey model = B.concat ["global:", model, ":id"]

read :: Lens' (DbLayer b) (Snaplet RedisDB) -> ByteString -> ByteString -> Handler b (DbLayer b) (M.Map ByteString ByteString)
read r model objId = runRedisDB r $ do
  let key = objKey model objId
  Right res  <- fmap M.fromList <$> hgetall key
  return res

read' :: Lens' (DbLayer b) (Snaplet RedisDB) -> ByteString -> Handler b (DbLayer b) (M.Map ByteString ByteString)
read' r objId = runRedisDB r $ do
  Right res  <- fmap M.fromList <$> hgetall objId
  return res

readAll :: Lens' (DbLayer b) (Snaplet RedisDB) -> ModelName -> Handler b (DbLayer b) [M.Map ByteString ByteString]
readAll r model = runRedisDB r $ do
  Right ids <- keys $ B.concat [model, ":*"]
  res <- forM ids $ \i ->
    fmap (M.insert "id" i . M.fromList) <$> hgetall i
  return $ rights res

create :: Lens' (DbLayer b) (Snaplet RedisDB) -> ModelName -> Object -> Handler b (DbLayer b) ByteString
create r model commit = runRedisDB r $ do
  Right n <- incr $ modelIdKey model
  let idStr   = B.pack $ show n
  let key     = objKey model idStr
  let commit' = M.insert "id" idStr commit
  Right _ <- hmset key $ M.toList commit'
  return idStr

create' :: Lens' (DbLayer b) (Snaplet RedisDB) -> ModelName -> ByteString -> Object -> Handler b (DbLayer b) (M.Map ByteString ByteString)
create' r model id commit = runRedisDB r $ do
  let key = objKey model id
  Right _   <- hmset key $ M.toList $ M.insert "id" id commit
  Right res <- fmap M.fromList <$> hgetall key
  return res

update :: Lens' (DbLayer b) (Snaplet RedisDB) -> ModelName -> ByteString -> Object -> Handler b (DbLayer b) ()
update r model objId commit = runRedisDB r $ do
  let key = objKey model objId
  Right _ <- hmset key $ M.toList commit
  return ()

updateMany :: Lens' (DbLayer b) (Snaplet RedisDB) -> M.Map ByteString (M.Map ByteString ByteString) -> Handler b (DbLayer b) (Either Reply ())
updateMany r objectMap = runRedisDB r $ do
  res <- forM (M.toList objectMap) $ \(k,obj) ->
        if M.null obj then return $ Right undefined
                      else hmset k $ M.toList obj
  case lefts res of
    [] -> return $ Right ()
    _  -> error "updateMany failed"

delete :: Lens' (DbLayer b) (Snaplet RedisDB) -> ModelName -> ByteString -> Handler b (DbLayer b) ()
delete r model objId = runRedisDB r $ do
  let key = objKey model objId
  Right _ <- del [key]
  return ()
