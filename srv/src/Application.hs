{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Application where

import Data.Set (Set)
import Control.Concurrent.STM
import Control.Lens

import Data.Pool
import Database.PostgreSQL.Simple as Pg

import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session

------------------------------------------------------------------------------
import Snaplet.Auth.Class
import Snaplet.SiteConfig
import Snaplet.SiteConfig.Class
import Snaplet.DbLayer.Types (DbLayer)
import Snap.Snaplet.Vin
import Snaplet.FileUpload hiding (db)
import Snaplet.Geo
import Snap.Snaplet.SimpleLog
import Snaplet.Search

import RuntimeFlag

------------------------------------------------------------------------------
-- | Application snaplet state type: Redson, Heist.
data App = App
    { _heist      :: Snaplet (Heist App)
    , _session    :: Snaplet SessionManager
    , _auth       :: Snaplet (AuthManager App)
    , _siteConfig :: Snaplet (SiteConfig App)
    , _db         :: Snaplet (DbLayer App)
    , pg_search   :: Pool Pg.Connection
    , pg_actass   :: Pool Pg.Connection
    , _vin        :: Snaplet Vin
    , _fileUpload :: Snaplet (FileUpload App)
    , _geo        :: Snaplet Geo
    , feLog       :: Log
    , runtimeFlags:: TVar (Set RuntimeFlag)
    , _authDb     :: Snaplet Postgres
    , _search     :: Snaplet Search
    }


type AppHandler = Handler App App

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasAuth App where
  authLens = subSnaplet auth

instance HasSiteConfig App where
  siteConfigLens = subSnaplet siteConfig

instance MonadLog (Handler App App) where
  askLog = with db askLog

instance HasPostgres (Handler b App) where
  getPostgresState = with authDb get


withPG
  :: (App -> Pool Pg.Connection) -> (Pg.Connection -> IO res)
  -> AppHandler res
withPG pool f = gets pool >>= liftIO .(`withResource` f)
