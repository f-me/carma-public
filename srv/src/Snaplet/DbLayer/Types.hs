{-# LANGUAGE TemplateHaskell #-}

module Snaplet.DbLayer.Types where

import Data.Map (Map)
import Data.ByteString (ByteString)
import Data.Lens.Template

import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple (Postgres)
import Snap.Snaplet.RedisDB (RedisDB)


type ObjectId = ByteString
type Object = Map FieldName ByteString
type ObjectMap = Map ObjectId Object
type ModelName = ByteString
type FieldName = ByteString
type FieldValue = ByteString

type DbHandler b r = Handler b (DbLayer b) r

data DbLayer b = DbLayer
    {_redis      :: Snaplet RedisDB
    ,_postgres   :: Snaplet Postgres
    }

makeLens ''DbLayer


