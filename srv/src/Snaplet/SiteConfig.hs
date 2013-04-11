
module Snaplet.SiteConfig
  (initSiteConfig
  ,SiteConfig(..)
  ) where

import Control.Applicative
import Control.Monad
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

----------------------------------------------------------------------
import Snaplet.Auth.Class

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
      modifyResponse $ setContentType "application/json"
      writeModel name (stripModel (Right cu) m)

writeModel :: ByteString -> Model -> Handler b (SiteConfig b) ()
writeModel "contract" model = do
  field <- fromMaybe "showform" <$> getParam "field"
  pid   <- getParam "pid"
  when (pid == Nothing) $ finishWithError 401 "need pid param"
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
  => FilePath -> Pool Pg.Connection -> SnapletInit b (SiteConfig b)
initSiteConfig cfgDir pg_pool = makeSnaplet
  "site-config" "Site configuration storage"
  Nothing $ do -- ?
    addRoutes
      [("models",       method GET serveModels)
      ,("model/:name",  method GET serveModel)
      ,("dictionaries", method GET serveDictionaries)
      ]
    SiteConfig
      <$> liftIO (loadModels cfgDir)
      <*> liftIO (loadDictionaries cfgDir)
      <*> (return pg_pool)