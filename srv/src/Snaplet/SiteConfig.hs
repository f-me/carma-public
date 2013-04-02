{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Snaplet.SiteConfig
  ( SiteConfig
  , initSiteConfig
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Aeson as Aeson

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple

----------------------------------------------------------------------
import Snaplet.Auth.Class
import Snaplet.Auth.PGUsers

import Snaplet.SiteConfig.Types
import Snaplet.SiteConfig.Permissions
import Snaplet.SiteConfig.Models
import Snaplet.SiteConfig.Dictionaries


data SiteConfig b = SiteConfig
  { models       :: Map ModelName Model
  , dictionaries :: Aeson.Value
  , authDb       :: Lens' b (Snaplet Postgres)
  }


serveModels :: HasAuth b => Handler b (SiteConfig b) ()
serveModels = do
  mcu <- withAuth currentUser
  case mcu of
    Nothing -> do
      modifyResponse $ setResponseCode 401
      getResponse >>= finishWith
    Just cu -> do
      -- Substitute user roles stored in PG
      cu' <- (gets authDb >>=) . flip withTop $ replaceRolesFromPG cu
      ms <- gets models
      modifyResponse $ setContentType "application/json"
      writeLBS $ Aeson.encode
               $ M.map (stripModel $ Right cu') ms


serveDictionaries :: Handler b (SiteConfig b) ()
serveDictionaries = ifTop $ do
  ds <- gets dictionaries
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode ds


initSiteConfig :: HasAuth b =>
                  FilePath 
               -> Lens' b (Snaplet Postgres)
               -- ^ Lens to a snaplet with Postgres DB used to check
               -- user roles.
               -> SnapletInit b (SiteConfig b)
initSiteConfig cfgDir authDb = makeSnaplet
  "site-config" "Site configuration storage"
  Nothing $ do -- ?
    addRoutes
      [("models",       method GET serveModels)
      ,("dictionaries", method GET serveDictionaries)
      ]
    (mdls, dicts) <- liftIO $ 
                     (,) <$> loadModels cfgDir <*> loadDictionaries cfgDir
    return $ SiteConfig mdls dicts authDb
