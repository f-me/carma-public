{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Application

where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Class

import           Data.Text (Text)
import           Data.Map as Map

import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session

import           Carma.Model (IdentI)
import           Carma.Model.Usermeta (Usermeta)

import           DMCC

import qualified WeatherApi as W

import           Snaplet.Auth.Class
import           Snaplet.ChatManager
import           Snaplet.SiteConfig
import           Snaplet.SiteConfig.Class
import           Snaplet.TaskManager
import           Snaplet.FileUpload
import           Snaplet.Geo
import           Snaplet.Search
import           Snaplet.Messenger
import           Snaplet.Messenger.Class


-- | Global application options.
data AppOptions = AppOptions
    { localName       :: Maybe Text
      -- ^ Name of CaRMa installation (read from @local-name@ config
      -- option)
    , searchMinLength :: Int
      -- ^ Minimal query length for database-heavy searches
      -- (@search-min-length@).
    , dmccWsHost :: Maybe Text
    , dmccWsPort :: Int
    }


-- | CaRMa top-level application state.
data App = App
    { _heist      :: Snaplet (Heist App)
    , _session    :: Snaplet SessionManager
    , _auth       :: Snaplet (AuthManager App)
    , _siteConfig :: Snaplet (SiteConfig App)
    , _taskMgr    :: Snaplet (TaskManager App)
    , _fileUpload :: Snaplet (FileUpload App)
    , _chat       :: Snaplet (ChatManager App)
    , _geo        :: Snaplet (Geo App)
    , _db         :: Snaplet Postgres
    , _search     :: Snaplet (Search App)
    , options     :: AppOptions
    , _messenger  :: Snaplet Messenger
    , weatherCfg  :: W.Config
    , extMap      :: TVar (Map.Map Extension (IdentI Usermeta))
      -- ^ AVAYA extensions map
    }


type AppHandler = Handler App App

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasSiteConfig App where
  siteConfigLens = subSnaplet siteConfig

instance HasPostgresAuth App App where
  withAuth = with auth
  withAuthPg = with db

instance HasPostgres (Handler b App) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)

instance HasMsg App where
  messengerLens = subSnaplet messenger
