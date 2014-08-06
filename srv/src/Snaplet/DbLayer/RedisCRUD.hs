{-# LANGUAGE Rank2Types #-}
module Snaplet.DbLayer.RedisCRUD where

import Prelude hiding (read)
import Control.Lens
import Control.Monad (forM)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Functor
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M

import Database.Redis as R
import Snap.Snaplet
import Snap.Snaplet.RedisDB
import Snaplet.DbLayer.Types


objKey :: ModelName -> ObjectId -> ByteString
objKey model objId = T.encodeUtf8 $ T.concat [model, ":", objId]

modelIdKey :: ModelName -> ByteString
modelIdKey model = T.encodeUtf8 $ T.concat ["global:", model, ":id"]

trObj :: Map ByteString ByteString -> Object
trObj = M.map T.decodeUtf8 . M.mapKeys T.decodeUtf8

rtObj :: Object -> [(ByteString, ByteString)]
rtObj o = [(T.encodeUtf8 k, T.encodeUtf8 v) | (k,v) <- M.toList o]

read
  :: Lens' (DbLayer b) (Snaplet RedisDB)
  -> ModelName -> ObjectId
  -> Handler b (DbLayer b) Object
read r model objId = runRedisDB r $ do
  let key = objKey model objId
  Right res  <- fmap M.fromList <$> hgetall key
  return $ trObj res

read'
  :: Lens' (DbLayer b) (Snaplet RedisDB)
  -> ObjectId
  -> Handler b (DbLayer b) Object
read' r objId = runRedisDB r $ do
  Right res  <- fmap M.fromList <$> hgetall (T.encodeUtf8 objId)
  return $ trObj res

readAll
  :: Lens' (DbLayer b) (Snaplet RedisDB)
  -> ModelName
  -> Handler b (DbLayer b) [Object]
readAll r model = runRedisDB r $ do
  Right ids <- keys $ objKey model "*"
  res <- forM ids $ \i ->
    fmap (M.insert "id" i . M.fromList) <$> hgetall i
  return $ map trObj $ rights res

create
  :: Lens' (DbLayer b) (Snaplet RedisDB)
  -> ModelName -> Object
  -> Handler b (DbLayer b) ObjectId
create r model commit = runRedisDB r $ do
  Right n <- incr $ modelIdKey model
  let ident = T.pack $ show n
  let key   = objKey model ident
  Right _ <- hmset key $ ("id", T.encodeUtf8 ident) : rtObj commit
  return ident

create'
  :: Lens' (DbLayer b) (Snaplet RedisDB)
  -> ModelName -> ObjectId -> Object
  -> Handler b (DbLayer b) Object
create' r model ident commit = runRedisDB r $ do
  let key = objKey model ident
  Right _ <- hmset key $ ("id", T.encodeUtf8 ident) : rtObj commit
  Right res <- fmap M.fromList <$> hgetall key
  return $ trObj res

update
  :: Lens' (DbLayer b) (Snaplet RedisDB)
  -> ModelName -> ObjectId -> Object
  -> Handler b (DbLayer b) ()
update r model objId commit = runRedisDB r $ do
  let key = objKey model objId
  Right _ <- hmset key $ rtObj commit
  return ()

updateMany
  :: Lens' (DbLayer b) (Snaplet RedisDB)
  -> ObjectMap
  -> Handler b (DbLayer b) (Either Reply ())
updateMany r objectMap = runRedisDB r $ do
  res <- forM (M.toList objectMap) $ \(k,obj) ->
        if M.null obj then return $ Right undefined
                      else hmset (T.encodeUtf8 k) $ rtObj obj
  case lefts res of
    [] -> return $ Right ()
    _  -> error "updateMany failed"

delete
  :: Lens' (DbLayer b) (Snaplet RedisDB)
  -> ModelName -> ObjectId
  -> Handler b (DbLayer b) ()
delete r model objId = runRedisDB r $ do
  let key = objKey model objId
  Right _ <- del [key]
  return ()

exists :: Lens' (DbLayer b) (Snaplet RedisDB) 
       -> ModelName
       -> ObjectId
       -> Handler b (DbLayer b) Bool
exists r model objId = runRedisDB r $ do
  Right res <- R.exists $ objKey model objId
  return res

