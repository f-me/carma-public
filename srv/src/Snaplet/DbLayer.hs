{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Snaplet.DbLayer
  (create
  ,read
  ,read'
  ,selectDb
  ,update
  ,delete
  ,exists
  ,initDbLayer
  ) where

import Prelude hiding (read)
import Control.Applicative
import Control.Lens (Lens')
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Text          as T

import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple (Postgres, pgsInit)
import Snap.Snaplet.RedisDB (redisDBInitConf)

import qualified Snaplet.DbLayer.RedisCRUD as Redis
import qualified Snaplet.DbLayer.PostgresCRUD as Postgres
import Snaplet.DbLayer.Util (selectDb)

import Snaplet.DbLayer.Types
import qualified Carma.ModelTables as MT (loadTables)
import Snaplet.DbLayer.Triggers
import Snaplet.Auth.Class
import Snaplet.Messenger (sendMessage)
import Snaplet.Messenger.Class

import Util


create :: (HasAuth b, HasMsg b)
       => ModelName -> Object
       -> Handler b (DbLayer b) Object
create model commit = do
  tbls <- gets syncTables
  syslogJSON Debug "DbLayer/create" ["model" .= model, "commit" .= commit]
  --
  obj <- applyDefaults commit
  objId <- Redis.create redis model obj
  --
  let obj' = Map.insert "id" objId obj
  syslogJSON Debug "DbLayer/create" ["obj'" .= obj']
  --
  Postgres.insert tbls model obj'

  let result = Map.insert "id" objId $ obj Map.\\ commit
  syslogJSON Debug "DbLayer/create/result" ["result" .= result]
  return result


read :: ModelName -> ObjectId
     -> Handler b (DbLayer b) Object
read model objId = do
  res <- Redis.read redis model objId
  -- FIXME: catch NotFound => search in postgres
  return res

read' :: ObjectId -> Handler b (DbLayer b) Object
read' = Redis.read' redis

update :: (HasAuth b, HasMsg b)
       =>  ModelName -> ObjectId -> Object
       -> Handler b (DbLayer b) Object
update model objId commit = do
  tbls <- gets syncTables
  syslogJSON Debug "DbLayer/update" ["model" .= model, "commit" .= commit]
  --
  let fullId = T.concat [model, ":", objId]
  -- FIXME: catch NotFound => transfer from postgres to redis
  -- (Copy on write)
  (futures, changes) <- triggerUpdate model objId commit
  Right _ <- Redis.updateMany redis changes
  --
  let
    toPair [x, y] = Just (x, y)
    toPair _ = Nothing

  let changes' = Map.mapWithKey (\(_,k) v -> Map.insert "id" k v) .
                 Map.mapKeys fromJust                             .
                 Map.filterWithKey (\k _ -> isJust k)             .
                 Map.mapKeys (toPair . T.splitOn ":")             $
                 changes
  syslogJSON Debug "DbLayer/update" ["changes" .= changes]
  Postgres.insertUpdateMany tbls changes'
  sequence_ futures -- run delayed actions

  -- mapM_ (uncurry $ Evt.logLegacyCRUD Update) $ Map.toList changes'
  --
  let stripUnchanged orig = Map.filterWithKey (\k v -> Map.lookup k orig /= Just v)
      commit' = stripUnchanged commit $ changes Map.! fullId

  withMsg $ sendMessage fullId commit'
  syslogJSON Debug "DbLayer/update/result" ["result" .= commit']
  return commit'

delete :: ModelName -> ObjectId -> Handler b (DbLayer b) ()
delete model objId = Redis.delete redis model objId


exists :: ModelName -> ObjectId -> Handler b (DbLayer b) Bool
exists model objId = Redis.exists redis model objId


-- TODO Use lens to an external AuthManager
initDbLayer :: Snaplet (AuthManager b)
            -> Lens' b (Snaplet Postgres)
            -- ^ Lens to a snaplet with Postgres DB used for user
            -- authorization.
            -> FilePath
            -> SnapletInit b (DbLayer b)
initDbLayer sessionMgr adb _ = makeSnaplet "db-layer" "Storage abstraction"
  Nothing $ do
    -- syslog Info "Server started"
    tbls <- liftIO $ MT.loadTables "resources/site-config/models"

    DbLayer adb
      <$> nestSnaplet "redis" redis redisDBInitConf
      <*> nestSnaplet "pgsql" postgres pgsInit
      <*> pure sessionMgr
      <*> (return tbls)
