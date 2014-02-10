{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Application where

import Control.Concurrent.STM
import Control.Lens

import Data.Pool
import Database.PostgreSQL.Simple as Pg

import Data.Set (Set)
import Data.Text (Text)

import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session
import Snap.Snaplet.SimpleLog

import Snaplet.Auth.Class
import Snaplet.SiteConfig
import Snaplet.SiteConfig.Class
import Snaplet.DbLayer.Types (DbLayer)
import Snaplet.TaskManager
import Snaplet.FileUpload hiding (db)
import Snaplet.Geo
import Snaplet.Search

import RuntimeFlag


-- | Global application options.
data AppOptions = AppOptions
    { localName       :: Maybe Text
      -- ^ Name of CaRMa installation (read from @local-name@ config
      -- option)
    , searchMinLength :: Int
      -- ^ Minimal query length for database-heavy searches
      -- (@search-min-length@).
    }


-- | CaRMa top-level application state.
data App = App
    { _heist      :: Snaplet (Heist App)
    , _session    :: Snaplet SessionManager
    , _auth       :: Snaplet (AuthManager App)
    , _siteConfig :: Snaplet (SiteConfig App)
    , _db         :: Snaplet (DbLayer App)
    , pg_search   :: Pool Pg.Connection
    , pg_actass   :: Pool Pg.Connection
    , _taskMgr    :: Snaplet (TaskManager App)
    , _fileUpload :: Snaplet (FileUpload App)
    , _geo        :: Snaplet Geo
    , feLog       :: Log
    , runtimeFlags:: TVar (Set RuntimeFlag)
    , _authDb     :: Snaplet Postgres
    , _search     :: Snaplet (Search App)
    , options     :: AppOptions
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
