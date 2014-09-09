{-# LANGUAGE Rank2Types, ScopedTypeVariables, PatternGuards #-}

module Snaplet.SiteConfig
  ( SiteConfig
  , initSiteConfig
  ) where

import Control.Applicative
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.State

import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
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

import Snaplet.DbLayer.Types
import Snaplet.SiteConfig.Config
import Snaplet.SiteConfig.SpecialPermissions
import Snaplet.SiteConfig.Models
import Snaplet.SiteConfig.Dictionaries

import AppHandlers.Util hiding (withPG)
import Utils.HttpErrors

import Data.Model.Sql
import qualified Data.Model as Model
import qualified Carma.Model as Model
import qualified Carma.Model.ServiceInfo as ServiceInfo


getModel :: ModelName -> Text -> Handler b (SiteConfig b) (Maybe Model)
getModel name view =
  case T.splitOn ":" view of
    ["ctr",pgm] ->
      case Model.dispatch name $ viewForModel "" of
        Just (Just res) -> Just <$> constructModel name pgm res
        -- Try to fetch a plain model if constructor failed
        _               -> getModel name ""
    _ -> case Model.dispatch name $ viewForModel view of
           Just res -> return res
           -- Try to obtain a new-style model using legacy model names
           -- if the supplied name is unknown
           Nothing
             | Just name' <- Map.lookup name Model.legacyModelNames ->
                 case Model.dispatch name' $ viewForModel view of
                   Just res -> return $ setModelName name <$> res
                   Nothing  -> Map.lookup name <$> gets models
           -- Serve an old-style model
           _ -> Map.lookup name <$> gets models


serveModel :: HasAuth b => Handler b (SiteConfig b) ()
serveModel = do
  Just name  <- getParamT "name"
  view  <- fromMaybe "" <$> getParamT "view"
  model <- getModel name view
  mcu   <- withAuth currentUser
  case (mcu, model) of
    (Nothing, _) -> finishWithError 401 ""
    (_, Nothing) -> finishWithError 404 "Unknown model/view"
    (Just cu, Just m) ->
      case view `elem` ["search", "portalSearch", "kpi"] of
        True  -> writeModel m
        False -> stripModel cu m >>= writeModel


viewForModel :: forall m . Model.Model m => T.Text -> m -> Maybe Model
viewForModel name _
  = join
  $ fmap (Aeson.decode . Aeson.encode)
  $ (Model.modelView name :: Maybe (Model.ModelView m))


setModelName :: Text -> Model -> Model
setModelName n m = m {modelName = n}


-- | Combine field data and writable flag to a new readonly meta value.
ro :: Field -> Bool -> Bool
ro fld wr =
  case Map.lookup "readonly" $ fromMaybe Map.empty $ meta fld of
    Just (Aeson.Bool True) -> True
    _                      -> not wr


constructModel
  :: Text -> Text -> Model
  -> Handler b (SiteConfig b) Model
constructModel mdlName program model = do
  let q = [sql|
      select c.field, c.label, c.r, c.w, c.required, c.info, c.ord
        from "ConstructorFieldOption" c, "CtrModel" m
        where m.id = c.model
          and m.value = ?
          and program = ? :: int
        order by ord asc
      |]
  pg <- gets pg_search
  res <- liftIO (withResource pg $ \c -> query c q [mdlName,program])
  let optMap = Map.fromList [(nm,(l,r,w,rq,inf,o)) | (nm,l,r,w,rq,inf,o) <- res]
  let adjustField f = case Map.lookup (name f) optMap of
        Nothing -> [f] -- NB: field is not modified if no options found
        Just (_,False,_,_,_,_) -> [] -- unreadable field
        Just (l,True,w,rq,inf,o) ->
          [f {meta
              = Map.insert "label"    (Aeson.String l)
              . Map.insert "required" (Aeson.Bool rq)
              . Map.insert "infoText" (Aeson.String inf)
              . Map.insert "readonly" (Aeson.Bool $ ro f w)
              <$> meta f
            , sortingOrder = o
            , canWrite = w}
          ]
  return $ model
    {fields = sortBy (comparing sortingOrder)
        $ concatMap adjustField
        $ fields model
    }


writeModel :: HasAuth b => Model -> Handler b (SiteConfig b) ()
writeModel model
  = writeJSON
  =<< case modelName model of
    -- If sid parameter is specified for Contract model, serve model
    -- for portal screen table/form.
    "Contract" -> do
      sid <- getParamT "sid"
      case sid of
        Just i -> do
          field <- fromMaybe "showform" <$> getParamT "field"
          case field of
            "showform"  -> stripContract model i Form
            "showtable" -> stripContract model i Table
            _ ->
              finishWithError 403 "field param be either showform or showtable"
        Nothing -> return model
    _ -> return model



stripModel :: AuthUser -> Model -> Handler b (SiteConfig b) Model
stripModel u m = do
  let Just uid = userId u
      -- When requesting an old-style model (@case@), use permissions
      -- defined for new-style model (@Case@).
      fixModelName v =
          fromMaybe v $ Map.lookup v Model.legacyModelNames
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
    (unUid uid, fixModelName $ modelName m)
  let fieldsMap = Map.fromList readableFields
  let fieldFilter f fs = case Map.lookup (name f) fieldsMap of
        Nothing -> fs
        Just wr ->
          let w = canWrite f && wr
          in f {meta = Map.insert "readonly" (Aeson.Bool $ ro f w) <$> meta f
               ,canWrite = w
               } : fs
  return $ m {fields = foldr fieldFilter [] $ fields m}

-- | Serve available idents for a model (given in @name@ request
-- parameter) as JSON object: @{"foo": 12, "bar": 28}@.
serveIdents :: Handler b (SiteConfig b) ()
serveIdents = do
  nm <- getParamT "name"
  case nm of
    Nothing -> error "Must provide model name"
    Just name ->
        let
            fun :: Model.Model m =>
                   m
                -> HM.HashMap String (Model.IdentI m)
            fun _ = Model.idents
            idents' = Model.dispatch name (Aeson.encode . fun)
        in
          case idents' of
            Just e -> (modifyResponse $ setContentType "application/json") >>
                      writeLBS e
            Nothing -> handleError 404

serveDictionaries :: Handler b (SiteConfig b) ()
serveDictionaries = do
  let withPG f = gets pg_search >>= liftIO . (`withResource` f)

  serviceInfos <- withPG
    $ selectJSON (ServiceInfo.program :. ServiceInfo.service :. ServiceInfo.info)

  Aeson.Object dictMap <- gets dictionaries
  -- Support legacy client interface for some dictionaries
  writeJSON $ Aeson.Object
    $ HM.insert "ServiceInfo"
      (Aeson.object [("entries", Aeson.Array $ V.fromList serviceInfos)])
      dictMap


initSiteConfig :: HasAuth b
                  => FilePath
                  -> Pool Pg.Connection
                  -> Lens' b (Snaplet (DbLayer b))
                  -> SnapletInit b (SiteConfig b)
initSiteConfig cfgDir pg_pool db = makeSnaplet
  "site-config" "Site configuration storage"
  Nothing $ do
    addRoutes
      [ ("model/:name",  method GET serveModel)
      , ("idents/:name", method GET serveIdents)
      , ("dictionaries", method GET serveDictionaries)
      ]
    mdls <- liftIO $ loadModels cfgDir
    dicts <- liftIO $ loadDictionaries cfgDir
    return $ SiteConfig mdls dicts pg_pool db
