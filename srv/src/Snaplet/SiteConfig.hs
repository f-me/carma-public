
{-# LANGUAGE ScopedTypeVariables #-}

module Snaplet.SiteConfig
  ( SiteConfig
  , initSiteConfig
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

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
import Snaplet.SiteConfig.FakeModels
import Snaplet.SiteConfig.Dictionaries

import Utils.HttpErrors

import Data.Model.Sql
import qualified Data.Model as Model
import qualified Carma.Model as Model
import qualified Carma.Model.Program as Program
import qualified Carma.Model.Role as Role


writeJSON :: Aeson.ToJSON v => v -> Handler a b ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v


serveModel :: HasAuth b => Handler b (SiteConfig b) ()
serveModel = do
  Just name  <- getParam "name"
  view  <-  T.decodeUtf8 <$> fromMaybe "" <$> getParam "view"
  model <- getParam "arg" >>= \arg ->
    case T.splitOn ":" . T.decodeUtf8 <$> arg of
      Just ["newCase",pgm] -> fmap Just
        $ case name of
          "case" -> newCase pgm
          _      -> newSvc pgm name
      _ -> case Model.dispatch (T.decodeUtf8 name) $ viewForModel view of
        Just res -> return res
        Nothing  -> M.lookup name <$> gets models

  mcu   <- withAuth currentUser
  case return (,) `ap` mcu `ap` model of
    Nothing -> finishWithError 401 ""
    Just (cu, m) -> stripModel cu m >>= writeModel

viewForModel :: forall m . Model.Model m => T.Text -> m -> Maybe Model
viewForModel name _
  = Aeson.decode $ Aeson.encode (Model.modelView name :: Model.ModelView m)

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
        and p.role::text = ANY (u.roles)
      group by p.field
    |]
    (unUid uid, modelName m)
  let fieldsMap = M.fromList readableFields
  let fieldFilter f fs = case M.lookup (name f) fieldsMap of
        Nothing -> fs
        Just wr -> f {canWrite = canWrite f && wr} : fs
  return $ m {fields = foldr fieldFilter [] $ fields m}


serveDictionaries :: Handler b (SiteConfig b) ()
serveDictionaries = do
  let withPG f = gets pg_search >>= liftIO . (`withResource` f)
  programs <- withPG $ selectJSON
    (Program.value :. Program.label :. eq Program.active True)
  roles <- withPG $ selectJSON $
           (Model.ident :: Model.IdentF Role.Role) :.
           Role.label
  let roles' =
          map (\(Aeson.Object o) ->
               Aeson.Object $ HM.insert "value" (o HM.! "id") o)
          roles
  Aeson.Object dictMap <- gets dictionaries
  writeJSON $ Aeson.Object
    $ HM.insert "Roles"
      (Aeson.object [("entries", Aeson.Array $ V.fromList roles')])
    $ HM.insert "Programs"
      (Aeson.object [("entries", Aeson.Array $ V.fromList programs)])
      dictMap


initSiteConfig :: HasAuth b
                  => FilePath
                  -> Pool Pg.Connection
                  -> SnapletInit b (SiteConfig b)
initSiteConfig cfgDir pg_pool = makeSnaplet
  "site-config" "Site configuration storage"
  Nothing $ do
    addRoutes
      [("model/:name",  method GET serveModel)
      ,("dictionaries", method GET serveDictionaries)
      ]
    liftIO $ SiteConfig
      <$> loadModels cfgDir
      <*> loadDictionaries cfgDir
      <*> pure pg_pool
