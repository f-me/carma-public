
{-# LANGUAGE TemplateHaskell #-}

module Snaplet.SiteConfig
  (initSiteConfig
  ,SiteConfig(..)
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

import Data.Lens.Template
import qualified Data.Aeson as Aeson

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth

----------------------------------------------------------------------
import Snaplet.SiteConfig.Types
import Snaplet.SiteConfig.Permissions
import Snaplet.SiteConfig.Models
import Snaplet.SiteConfig.Dictionaries
import Snaplet.SiteConfig.Triggers


data SiteConfig b = SiteConfig
  {_topAuth     :: Snaplet (AuthManager b)
  ,models       :: Map ModelName Model
  ,dictionaries :: Aeson.Value
  ,triggers     :: TriggersConfig
  }

makeLens ''SiteConfig

serveModels :: Handler b (SiteConfig b) ()
serveModels = do
  mcu <- with topAuth currentUser
  case mcu of
    Nothing -> do
      modifyResponse $ setResponseCode 401
      getResponse >>= finishWith
    Just cu -> do
      ms <- gets models
      modifyResponse $ setContentType "application/json"
      writeLBS $ Aeson.encode
               $ M.map (stripModel $ Right cu) ms

serveDictionaries :: Handler b (SiteConfig b) ()
serveDictionaries = ifTop $ do
  ds <- gets dictionaries
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode ds

initSiteConfig
  :: Snaplet (AuthManager b) -> FilePath
  -> SnapletInit b (SiteConfig b)
initSiteConfig topAuth' cfgDir = makeSnaplet
  "site-config" "Site configuration storage"
  Nothing $ do -- ?
    addRoutes
      [("models",       method GET serveModels)
      ,("dictionaries", method GET serveDictionaries)
      ]
    SiteConfig
      <$> pure topAuth'
      <*> liftIO (loadModels cfgDir)
      <*> liftIO (loadDictionaries cfgDir)
      <*> pure triggersConfig
