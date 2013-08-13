
module Snaplet.SiteConfig
  ( SiteConfig
  , initSiteConfig
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Aeson as Aeson

import Data.Pool
import Database.PostgreSQL.Simple as Pg
import Database.PostgreSQL.Simple.SqlQQ

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth

----------------------------------------------------------------------
import Snaplet.Auth.Class

import Snaplet.SiteConfig.Config
import Snaplet.SiteConfig.SpecialPermissions
import Snaplet.SiteConfig.Models
import Snaplet.SiteConfig.Dictionaries

import Utils.HttpErrors


writeJSON :: Aeson.ToJSON v => v -> Handler a b ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v

serveModel :: HasAuth b => Handler b (SiteConfig b) ()
serveModel = do
  mcu   <- withAuth currentUser
  name  <- fromJust <$> getParam "name"
  model <- M.lookup name <$> gets models
  case return (,) `ap` mcu `ap` model of
    Nothing -> finishWithError 401 ""
    Just (cu, m) -> stripModel cu m >>= writeModel


writeModel :: Model -> Handler b (SiteConfig b) ()
writeModel model
  = writeJSON
  =<< case modelName model of
    "contract" -> do
      field <- fromMaybe "showform" <$> getParam "field"
      when (field /= "showform" && field /= "showtable") $
        finishWithError 403 "field param should have showform of showtable value"
      pid   <- getParam "pid"
      when (pid == Nothing) $ finishWithError 403 "need pid param"
      stripContract model (fromJust pid) field
    _ -> return model



stripModel :: AuthUser -> Model -> Handler b (SiteConfig b) Model
stripModel u m = do
  let Just uid = userId u
  let withPG f = gets pg_search >>= liftIO . (`withResource` f)
  readableFields <- withPG $ \c -> query c [sql|
    select p.field, max(p.w::int)::bool
      from "FieldPermission" p, usermetatbl u
      where u.uid = ?::int
        and p.model = ?
        and p.r = true
        and p.role = ANY (u.roles)
      group by p.field
    |]
    (unUid uid, modelName m)
  let fieldsMap = M.fromList readableFields
  let fieldFilter f fs = case M.lookup (name f) fieldsMap of
        Nothing -> fs
        Just wr -> f {canWrite = wr} : fs
  return $ m {fields = foldr fieldFilter [] $ fields m}


serveDictionaries :: Handler b (SiteConfig b) ()
serveDictionaries = gets dictionaries >>= writeJSON


initSiteConfig :: HasAuth b
                  => FilePath
                  -> Pool Pg.Connection
                  -> SnapletInit b (SiteConfig b)
initSiteConfig cfgDir pg_pool = makeSnaplet
  "site-config" "Site configuration storage"
  Nothing $ do -- ?
    addRoutes
      [("model/:name",  method GET serveModel)
      ,("dictionaries", method GET serveDictionaries)
      ]
    liftIO $ SiteConfig
      <$> loadModels cfgDir
      <*> loadDictionaries cfgDir
      <*> pure pg_pool
