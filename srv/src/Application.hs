{-# LANGUAGE TemplateHaskell #-}

module Application where

import Data.Lens.Template

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
------------------------------------------------------------------------------
import Snaplet.SiteConfig
import Snaplet.DbLayer
import Snap.Snaplet.Vin
import Snap.Snaplet.AvayaAES


------------------------------------------------------------------------------
-- | Application snaplet state type: Redson, Heist.
data App = App
    { _heist      :: Snaplet (Heist App)
    , _session    :: Snaplet SessionManager
    , _auth       :: Snaplet (AuthManager App)
    , _siteConfig :: Snaplet (SiteConfig App)
    , _db         :: Snaplet (DbLayer App)
    , _vin        :: Snaplet Vin
    , _avaya      :: Snaplet (Avayaplet App)
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
  heistLens = subSnaplet heist
