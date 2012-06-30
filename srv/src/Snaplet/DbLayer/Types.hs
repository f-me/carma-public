{-# LANGUAGE TemplateHaskell #-}

module Snaplet.DbLayer.Types where

import Data.Map (Map)
import Data.ByteString (ByteString)
import Data.Lens.Template

import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple (Postgres)
import Snap.Snaplet.RedisDB (RedisDB)
import Snaplet.DbLayer.Indices

import qualified Fdds as Fdds

type ObjectId = ByteString
type Object = Map FieldName ByteString
type ObjectMap = Map ObjectId Object
type ModelName = ByteString
type FieldName = ByteString
type FieldValue = ByteString

type DbHandler b r = Handler b (DbLayer b) r

data DbLayer b = DbLayer
    {_redis    :: Snaplet RedisDB
    ,_postgres :: Snaplet Postgres
    ,triggers  :: TriggersConfig
    ,indices   :: Indices
    ,fdds      :: Fdds.Conf
    }

data TriggersConfig = TriggersConfig
  {recommendations :: Map ModelName (Map FieldName (Map FieldValue Object))
  }

makeLens ''DbLayer


