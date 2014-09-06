{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Snaplet.DbLayer.Types where

import Data.Map (Map)
import Data.Text (Text)

import Control.Lens

import Snap
import Snaplet.Auth.Class
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple (Postgres, HasPostgres(..))
import Snap.Snaplet.RedisDB (RedisDB)
import Carma.ModelTables (TableDesc)

type ObjectId = Text
type FieldName = Text
type FieldValue = Text
type Object = Map FieldName FieldValue
type ObjectMap = Map ObjectId Object
type ModelName = Text

type ProgramName = Text

type DbHandler b r = Handler b (DbLayer b) r

data DbLayer b = DbLayer
    {authDb    :: Lens' b (Snaplet Postgres)
    ,_redis    :: Snaplet RedisDB
    ,_postgres :: Snaplet Postgres
    ,_auth     :: Snaplet (AuthManager b)
    ,syncTables :: [TableDesc]
    }

makeLenses ''DbLayer

instance HasPostgres (Handler b (DbLayer b)) where
    getPostgresState = with postgres get

instance WithCurrentUser (Handler b (DbLayer b)) where
    withCurrentUser = with auth currentUser
