{-# LANGUAGE TemplateHaskell #-}

module Application where

import Data.Lens.Template

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
import Snaplet.SiteConfig
import Snap.Snaplet.RedisDB
import Snap.Snaplet.Vin
import Snap.Snaplet.AvayaAES


------------------------------------------------------------------------------
-- | Application snaplet state type: Redson, Heist.
data App = App
    { _heist      :: Snaplet (Heist App)
    , _session    :: Snaplet SessionManager
    , _auth       :: Snaplet (AuthManager App)
    , _siteConfig :: Snaplet (SiteConfig App)
    , _redis      :: Snaplet RedisDB
    , _postgres   :: Snaplet Postgres
    , _vin        :: Snaplet Vin
    , _avaya      :: Snaplet (Avayaplet App)
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

