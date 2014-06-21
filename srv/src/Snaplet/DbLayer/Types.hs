{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Snaplet.DbLayer.Types where

import Data.Map (Map)
import Data.ByteString (ByteString)

import Control.Concurrent.STM
import Control.Lens

import Snap
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple (Postgres, HasPostgres(..))
import Snap.Snaplet.RedisDB (RedisDB)
import Carma.ModelTables (TableDesc)

import qualified Database.PostgreSQL.Sync.Base as SM

import qualified WeatherApi as W

import DictionaryCache

type ObjectId = ByteString
type FieldName = ByteString
type FieldValue = ByteString
type Object = Map FieldName FieldValue
type ObjectMap = Map ObjectId Object
type ModelName = ByteString

type ProgramName = ByteString
type RKCName     = ByteString
type RKCValue    = Double
type RKCEntry    = Map RKCName RKCValue
type RKCCalc     = Map ProgramName RKCEntry

type DbHandler b r = Handler b (DbLayer b) r

data DbLayer b = DbLayer
    {authDb    :: Lens' b (Snaplet Postgres)
    ,_redis    :: Snaplet RedisDB
    ,_postgres :: Snaplet Postgres
    ,_auth     :: Snaplet (AuthManager b)
    ,syncRelations :: SM.Relations
    ,syncTables :: [TableDesc]
    ,dictCache :: TVar DictCache
    ,weather   :: W.Config
    ,rkcDict   :: RKCCalc
    }

makeLenses ''DbLayer

instance HasPostgres (Handler b (DbLayer b)) where
    getPostgresState = with postgres get


getDict :: (DictCache -> dict) -> Handler b (DbLayer b) dict
getDict dict
  = gets dictCache
  >>= fmap dict . liftIO . readTVarIO
