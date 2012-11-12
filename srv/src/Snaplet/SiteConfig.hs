
module Snaplet.SiteConfig
  (initSiteConfig
  ,SiteConfig(..)
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Aeson as Aeson

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth

----------------------------------------------------------------------
import Snaplet.Auth.Class

import Snaplet.SiteConfig.Types
import Snaplet.SiteConfig.Permissions
import Snaplet.SiteConfig.Models
import Snaplet.SiteConfig.Dictionaries


data SiteConfig b = SiteConfig
  {models       :: Map ModelName Model
  ,dictionaries :: Aeson.Value
  }


serveModels :: HasAuth b => Handler b (SiteConfig b) ()
serveModels = do
  mcu <- withAuth currentUser
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
  :: HasAuth b
  => FilePath -> SnapletInit b (SiteConfig b)
initSiteConfig cfgDir = makeSnaplet
  "site-config" "Site configuration storage"
  Nothing $ do -- ?
    addRoutes
      [("models",       method GET serveModels)
      ,("dictionaries", method GET serveDictionaries)
      ]
    SiteConfig
      <$> liftIO (loadModels cfgDir)
      <*> liftIO (loadDictionaries cfgDir)
