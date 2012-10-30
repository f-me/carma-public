{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Application where

import Data.Text (Text)
import Data.Map (Map)
import Data.Time.Clock (UTCTime)
import Data.Lens.Template
import Control.Concurrent.STM

import Data.Pool
import Database.PostgreSQL.Simple as Pg

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Class
import Snap.Snaplet.Session
------------------------------------------------------------------------------
import Snaplet.SiteConfig
import Snaplet.SiteConfig.Class
import Snaplet.DbLayer.Types
import Snap.Snaplet.Vin
import Snaplet.FileUpload
import Snap.Snaplet.SimpleLog

import Util (UsersDict)
------------------------------------------------------------------------------
-- | Application snaplet state type: Redson, Heist.
data App = App
    { _heist      :: Snaplet (Heist App)
    , _session    :: Snaplet SessionManager
    , _auth       :: Snaplet (AuthManager App)
    , loggedUsers :: TVar (Map Text (UTCTime, AuthUser))
    , allUsers    :: UsersDict
    , actionsLock :: TMVar ()
    , _siteConfig :: Snaplet (SiteConfig App)
    , _db         :: Snaplet (DbLayer App)
    , _pg_search  :: Pool Pg.Connection
    , _vin        :: Snaplet Vin
    , _fileUpload :: Snaplet FileUpload
    , feLog       :: Log
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasAuth App where
  authLens = subSnaplet auth

instance HasSiteConfig App where
  siteConfigLens = subSnaplet siteConfig

instance MonadLog (Handler App App) where
  askLog = with db askLog
