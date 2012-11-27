{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Snaplet.DbLayer.Types where

import Control.Monad.State (get)
import Data.Map (Map)
import Data.ByteString (ByteString)
import Data.Lens.Template
import Control.Concurrent.STM

import Snap
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple (Postgres, HasPostgres(..))
import Snap.Snaplet.RedisDB (RedisDB)
import Snaplet.DbLayer.Indices
import Carma.ModelTables (TableDesc)
import Snap.Snaplet.SimpleLog

import qualified Database.PostgreSQL.Sync.Base as SM

import qualified WeatherApi as W

import qualified Fdds as Fdds
import DictionaryCache
import Util (UsersDict)

type ObjectId = ByteString
type Object = Map FieldName ByteString
type ObjectMap = Map ObjectId Object
type ModelName = ByteString
type FieldName = ByteString
type FieldValue = ByteString

type ProgramName = ByteString
type RKCName     = ByteString
type RKCValue    = Double
type RKCEntry    = Map RKCName RKCValue
type RKCCalc     = Map ProgramName RKCEntry

type DbHandler b r = Handler b (DbLayer b) r

data DbLayer b = DbLayer
    {_redis    :: Snaplet RedisDB
    ,_postgres :: Snaplet Postgres
    ,_dbLog    :: Snaplet SimpleLog
    ,triggers  :: TriggersConfig
    ,indices   :: Indices
    ,fdds      :: Fdds.Conf
    ,syncRelations :: SM.Relations
    ,syncTables :: [TableDesc]
    ,allUsers  :: UsersDict
    ,dictCache :: TVar DictCache
    ,weather   :: W.Config
    ,rkcDict   :: RKCCalc
    }

data TriggersConfig = TriggersConfig
  {recommendations :: Map ModelName (Map FieldName (Map FieldValue Object))
  }

makeLens ''DbLayer

instance HasPostgres (Handler b (DbLayer b)) where
    getPostgresState = with postgres get

instance MonadLog (Handler b (DbLayer b)) where
    askLog = with dbLog askLog

getDict :: (DictCache -> dict) -> Handler b (DbLayer b) dict
getDict dict
  = gets dictCache
  >>= fmap dict . liftIO . readTVarIO
