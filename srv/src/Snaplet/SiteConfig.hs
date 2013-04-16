{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Snaplet.SiteConfig
  ( SiteConfig
  , initSiteConfig
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)

import Data.Pool
import Database.PostgreSQL.Simple as Pg

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple

----------------------------------------------------------------------
import Snaplet.Auth.Class
import Snaplet.Auth.PGUsers

import Snaplet.DbLayer.Types (DbLayer)

import Snaplet.SiteConfig.Config
import Snaplet.SiteConfig.Permissions
import Snaplet.SiteConfig.SpecialPermissions
import Snaplet.SiteConfig.Models
import Snaplet.SiteConfig.Dictionaries

import Utils.HttpErrors

serveModel :: HasAuth b => Handler b (SiteConfig b) ()
serveModel = do
  mcu   <- withAuth currentUser
  name  <- fromJust <$> getParam "name"
  model <- M.lookup name <$> gets models
  case return (,) `ap` mcu `ap` model of
    Nothing -> do
      modifyResponse $ setResponseCode 401
      getResponse >>= finishWith
    Just (cu, m) -> do
      cu' <- (gets authDb >>=) . flip withTop $ replaceMetaRolesFromPG cu
      modifyResponse $ setContentType "application/json"
      writeModel name (stripModel (Right cu') m)

writeModel :: ByteString -> Model -> Handler b (SiteConfig b) ()
writeModel "contract" model = do
  field <- fromMaybe "showform" <$> getParam "field"
  when (field /= "showform" && field /= "showtable") $
    finishWithError 403 "field param should have showform of showtable value"
  pid   <- getParam "pid"
  when (pid == Nothing) $ finishWithError 403 "need pid param"
  model' <- stripContract model (fromJust pid) field
  writeLBS $ Aeson.encode model'

writeModel _          model = writeLBS $ Aeson.encode model

serveModels :: HasAuth b => Handler b (SiteConfig b) ()
serveModels = do
  mcu <- withAuth currentUser
  case mcu of
    Nothing -> do
      modifyResponse $ setResponseCode 401
      getResponse >>= finishWith
    Just cu -> do
      cu' <- (gets authDb >>=) . flip withTop $ replaceMetaRolesFromPG cu
      ms <- gets models
      modifyResponse $ setContentType "application/json"
      writeLBS $ Aeson.encode
               $ M.map (stripModel $ Right cu') ms


serveDictionaries :: Handler b (SiteConfig b) ()
serveDictionaries = ifTop $ do
  ds <- gets dictionaries
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode ds

initSiteConfig :: HasAuth b
                  => FilePath
                  -> Pool Pg.Connection
                  -> Lens' b (Snaplet (DbLayer b))
                  -- ^ Lens to DbLayer snaplet used for user roles &
                  -- meta storage.
                  -> SnapletInit b (SiteConfig b)
initSiteConfig cfgDir pg_pool authDb = makeSnaplet
  "site-config" "Site configuration storage"
  Nothing $ do -- ?
    addRoutes
      [("models",       method GET serveModels)
      ,("model/:name",  method GET serveModel)
      ,("dictionaries", method GET serveDictionaries)
      ]
    (mdls, dicts) <- liftIO $ 
                     (,) <$> loadModels cfgDir <*> loadDictionaries cfgDir
    return $ SiteConfig mdls dicts pg_pool authDb
