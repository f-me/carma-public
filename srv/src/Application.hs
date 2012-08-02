{-# LANGUAGE TemplateHaskell #-}

module Application where

import Data.Text (Text)
import Data.Map (Map)
import Data.Lens.Template
import Control.Concurrent.STM

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
import Snap.Snaplet.AvayaAES
import Snaplet.FileUpload

------------------------------------------------------------------------------
-- | Application snaplet state type: Redson, Heist.
data App = App
    { _heist      :: Snaplet (Heist App)
    , _session    :: Snaplet SessionManager
    , _auth       :: Snaplet (AuthManager App)
    , loggedUsers :: TVar (Map Text ())
    , actionsLock :: TMVar ()
    , _siteConfig :: Snaplet (SiteConfig App)
    , _db         :: Snaplet (DbLayer App)
    , _vin        :: Snaplet Vin
    , _avaya      :: Snaplet (Avayaplet App)
    , _fileUpload :: Snaplet FileUpload
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasAuth App where
  authLens = subSnaplet auth

instance HasSiteConfig App where
  siteConfigLens = subSnaplet siteConfig
