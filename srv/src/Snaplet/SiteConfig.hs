
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

import AppHandlers.Util
import Utils.HttpErrors

import Data.Model.Sql
import qualified Data.Model as Model
import qualified Carma.Model as Model
import qualified Carma.Model.OldProgram as OldProgram
import qualified Carma.Model.Program as Program
import qualified Carma.Model.SubProgram as SubProgram
import qualified Carma.Model.ProgramInfo as ProgramInfo
import qualified Carma.Model.ServiceInfo as ServiceInfo
import qualified Carma.Model.ServiceNames as ServiceNames
import qualified Carma.Model.Colors as Colors
import qualified Carma.Model.Role as Role


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
    Just (cu, m) ->
      case name of
        ("Case") -> writeModel m
        ("Service") -> writeModel m
        ("Towage") -> writeModel m
        _        -> stripModel cu m >>= writeModel

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

-- | Serve available idents for a model (given in @name@ request
-- parameter) as JSON object.
serveIdents :: Handler b (SiteConfig b) ()
serveIdents = do
  nm <- getParam "name"
  case nm of
    Nothing -> error "Must provide model name"
    Just name ->
        let
            fun :: Model.Model m =>
                   m
                -> HM.HashMap String (Model.IdentI m)
            fun _ = Model.idents
            idents' = Model.dispatch (T.decodeUtf8 name) (Aeson.encode . fun)
        in
          case idents' of
            Just e -> (modifyResponse $ setContentType "application/json") >>
                      writeLBS e
            Nothing -> handleError 404

serveDictionaries :: Handler b (SiteConfig b) ()
serveDictionaries = do
  let withPG f = gets pg_search >>= liftIO . (`withResource` f)

  roles <- withPG $ selectJSON (Role.ident :. Role.label)
  let roles' =
          map (\(Aeson.Object o) ->
               Aeson.Object $ HM.insert "value" (o HM.! "id") o)
          roles

  -- Load old programs dictionary proxy
  oldPrograms <- withPG $ selectJSON
    (OldProgram.ident :. OldProgram.value :. OldProgram.label :. eq OldProgram.active True)

  -- TODO Calculate program active status from subprograms
  programs <- withPG $ selectJSON
    (Program.ident :. Program.label)
  subprograms <- withPG $ selectJSON
    (SubProgram.ident :. SubProgram.label :. eq SubProgram.active True)

  programInfos <- withPG
    $ selectJSON (ProgramInfo.program :. ProgramInfo.info)
  serviceInfos <- withPG
    $ selectJSON (ServiceInfo.program :. ServiceInfo.service :. ServiceInfo.info)
  serviceNames <- withPG
    $ selectJSON (ServiceNames.ident :. ServiceNames.value :. ServiceNames.label :. ServiceNames.icon)
  colors <- withPG
    $ selectJSON (Colors.ident :. Colors.value :. Colors.label)

  Aeson.Object dictMap <- gets dictionaries
  writeJSON $ Aeson.Object
    $ HM.insert "Roles"
      (Aeson.object [("entries", Aeson.Array $ V.fromList roles')])
    $ HM.insert "Programs"
      (Aeson.object [("entries", Aeson.Array $ V.fromList oldPrograms)])
    $ HM.insert "Program"
      (Aeson.object [("entries", Aeson.Array $ V.fromList programs)])
    $ HM.insert "SubProgram"
      (Aeson.object [("entries", Aeson.Array $ V.fromList subprograms)])
    $ HM.insert "ProgramInfo"
      (Aeson.object [("entries", Aeson.Array $ V.fromList programInfos)])
    $ HM.insert "ServiceInfo"
      (Aeson.object [("entries", Aeson.Array $ V.fromList serviceInfos)])
    $ HM.insert "ServiceNames"
      (Aeson.object [("entries", Aeson.Array $ V.fromList serviceNames)])
    $ HM.insert "Colors"
      (Aeson.object [("entries", Aeson.Array $ V.fromList colors)])
      dictMap


initSiteConfig :: HasAuth b
                  => FilePath
                  -> Pool Pg.Connection
                  -> SnapletInit b (SiteConfig b)
initSiteConfig cfgDir pg_pool = makeSnaplet
  "site-config" "Site configuration storage"
  Nothing $ do
    addRoutes
      [ ("model/:name",  method GET serveModel)
      , ("idents/:name", method GET serveIdents)
      , ("dictionaries", method GET serveDictionaries)
      ]
    liftIO $ SiteConfig
      <$> loadModels cfgDir
      <*> loadDictionaries cfgDir
      <*> pure pg_pool
