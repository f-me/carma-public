{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Application where

import Control.Lens

import Data.Pool
import Database.PostgreSQL.Simple as Pg
import Data.Text (Text)

import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session

import qualified WeatherApi as W

import Snaplet.Auth.Class
import Snaplet.SiteConfig
import Snaplet.SiteConfig.Class
import Snaplet.TaskManager
import Snaplet.FileUpload
import Snaplet.Geo
import Snaplet.Search
import Snaplet.Messenger
import Snaplet.Messenger.Class


-- | Global application options.
data AppOptions = AppOptions
    { localName       :: Maybe Text
      -- ^ Name of CaRMa installation (read from @local-name@ config
      -- option)
    , searchMinLength :: Int
      -- ^ Minimal query length for database-heavy searches
      -- (@search-min-length@).
    , cstaWsHost :: Maybe Text
    , cstaWsPort :: Int
    }


-- | CaRMa top-level application state.
data App = App
    { _heist      :: Snaplet (Heist App)
    , _session    :: Snaplet SessionManager
    , _auth       :: Snaplet (AuthManager App)
    , _siteConfig :: Snaplet (SiteConfig App)
    , pg_search   :: Pool Pg.Connection
    , pg_actass   :: Pool Pg.Connection
    , _taskMgr    :: Snaplet (TaskManager App)
    , _fileUpload :: Snaplet (FileUpload App)
    , _geo        :: Snaplet Geo
    , _db         :: Snaplet Postgres
    , _search     :: Snaplet (Search App)
    , options     :: AppOptions
    , _messenger  :: Snaplet Messenger
    , weatherCfg  :: W.Config
    }


type AppHandler = Handler App App

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasAuth App where
  authLens = subSnaplet auth

instance HasSiteConfig App where
  siteConfigLens = subSnaplet siteConfig

instance WithCurrentUser (Handler App App) where
  withCurrentUser = with auth currentUser

instance HasPostgres (Handler b App) where
  getPostgresState = with db get

instance HasMsg App where
  messengerLens = subSnaplet messenger
