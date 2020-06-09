{-# LANGUAGE Rank2Types, ScopedTypeVariables, PatternGuards #-}

module Snaplet.SiteConfig
  ( SiteConfig
  , initSiteConfig
  ) where

import Control.Lens (Lens')
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

import Database.PostgreSQL.Simple.SqlQQ

import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple hiding (field)
import Snap.Core
import Snap.Snaplet

----------------------------------------------------------------------
import Snaplet.Auth.Class

import Snaplet.SiteConfig.Config
import Snaplet.SiteConfig.SpecialPermissions
import Snaplet.SiteConfig.Models
import Snaplet.SiteConfig.Dictionaries

import Util
import Utils.HttpErrors

import qualified Data.Model as Model
import Data.Model.Utils.PostgreSQL.MSqlQQ hiding (parseQuery)
import qualified Carma.Model as Model
import qualified Carma.Model.FieldPermission as FieldPermission
import qualified Carma.Model.Usermeta as Usermeta


-- `Model` - is type from 'Snaplet.SiteConfig.Models'
-- `Model.Model` - is type from 'Data.Model'


getModel :: Text -> Text -> Handler b (SiteConfig b) (Maybe Model)
getModel name view =
  case T.splitOn ":" view of
    ["ctr", pgm] ->
      case Model.dispatch name $ viewForModel "" of
        Just (Just res) -> Just <$> constructModel name pgm res
        -- Try to fetch a plain model if constructor failed
        _               -> getModel name ""
    _ -> case Model.dispatch name $ viewForModel view of
           Just res -> return res
           Nothing -> finishWithError 404 "Unknown model/view"


serveModel :: HasPostgresAuth b (SiteConfig b) => Handler b (SiteConfig b) ()
serveModel = do
  Just name  <- getParamT "name"
  view  <- fromMaybe "" <$> getParamT "view"
  model <- getModel name view
  mcu   <- withAuth currentUser
  case (mcu, model) of
    (Nothing, _) -> finishWithError 401 ""
    (_, Nothing) -> finishWithError 404 "Unknown model/view"
    (Just cu, Just m) ->
      case view `elem` ["search", "portalSearch", "kpi", "searchCase"] of
        True  -> writeModel m
        False -> stripModel cu m >>= writeModel


viewForModel :: forall m . Model.Model m => T.Text -> m -> Maybe Model
viewForModel name _
  = join
  $ fmap (Aeson.decode . Aeson.encode)
  $ (Model.modelView name :: Maybe (Model.ModelView m))


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
  res <- withLens db $ query q [mdlName, program]
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
    { fields = sortBy (comparing sortingOrder)
        $ concatMap adjustField
        $ fields model
    }


writeModel :: HasPostgresAuth b (SiteConfig b) =>
              Model -> Handler b (SiteConfig b) ()
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
            _           -> finishWithError 403 "field param must be either\
                                               \ 'showform' or 'showtable'"
        Nothing -> return model
    _ -> return model


stripModel :: AuthUser -> Model -> Handler b (SiteConfig b) Model
stripModel u m = do

  fieldsMap <- inheritFields (modelName m) Map.empty

  let fieldFilter f fs = case Map.lookup (name f) fieldsMap of
        Nothing -> fs
        Just wr ->
          let w = canWrite f && wr
          in f { meta = Map.insert "readonly" (Aeson.Bool $ ro f w) <$> meta f
               , canWrite = w
               } : fs
  return $ m {fields = foldr fieldFilter [] $ fields m}

  where {-
          Inherit fields permissions from parent models.

          For instance you could describe a field for "Service"
          and then it would automatically be inherited by children models
          "Towage", "Taxi", "Tech" etc.

          If there's a record for the same field for parent and child both
          then child's one will override parent's.
        -}
        inheritFields :: Text
                      -> Map.Map Text Bool
                      -> Handler b (SiteConfig b) (Map.Map Text Bool)
        inheritFields name prevFieldsMap = do

          unitedFieldsMap <-
            Map.union prevFieldsMap <$> fieldsMapByModelName name

          let parentModelName = join $
                Model.dispatch name $ Model.parentName .
                  (const Model.modelInfo :: forall m . Model.Model m
                                         => m -> Model.ModelInfo m)

          case parentModelName of
               Just x  -> inheritFields x unitedFieldsMap
               Nothing -> pure unitedFieldsMap

        fieldsMapByModelName name =
          fmap Map.fromList $ withLens db $ uncurry query
            [msql|
              SELECT p.$(F|FieldPermission.field)$
                   , MAX( p.$(F|FieldPermission.w)$::INT )::BOOL
                FROM $(T|FieldPermission)$ AS p
                   , $(T|Usermeta)$ AS u
                WHERE u.$(F|Usermeta.uid)$
                        = $(V|unUid $ fromJust $ userId u)$::INT
                  AND p.$(F|FieldPermission.model)$ = $(V|name)$
                  AND p.$(F|FieldPermission.r)$ = TRUE
                  AND p.$(F|FieldPermission.role)$
                        = ANY (u.$(F|Usermeta.roles)$)
                GROUP BY p.$(F|FieldPermission.field)$
            |]


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
  Aeson.Object dictMap <- gets dictionaries
  writeJSON $ Aeson.Object dictMap


initSiteConfig :: HasPostgresAuth b (SiteConfig b)
                  => FilePath
                  -> Lens' b (Snaplet (AuthManager b))
                  -> Lens' b (Snaplet Postgres)
                  -> SnapletInit b (SiteConfig b)
initSiteConfig cfgDir a p = makeSnaplet
  "site-config" "Site configuration storage"
  Nothing $ do
    addRoutes
      [ ("model/:name",  method GET serveModel)
      , ("idents/:name", method GET serveIdents)
      , ("dictionaries", method GET serveDictionaries)
      ]
    dicts <- liftIO $ loadDictionaries cfgDir
    return $ SiteConfig dicts a p
