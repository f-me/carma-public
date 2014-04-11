{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

module Snaplet.SiteConfig
  ( SiteConfig
  , initSiteConfig
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

import Snaplet.SiteConfig.Config
import Snaplet.SiteConfig.SpecialPermissions
import Snaplet.SiteConfig.Models
import Snaplet.SiteConfig.Dictionaries

import AppHandlers.Util hiding (withPG)
import Utils.HttpErrors

import Data.Model.Sql
import qualified Data.Model as Model
import qualified Carma.Model as Model
import qualified Carma.Model.ProgramInfo as ProgramInfo
import qualified Carma.Model.ServiceInfo as ServiceInfo
import qualified Carma.Model.ServiceNames as ServiceNames

serveModelTrMap :: Handler b (SiteConfig b) ()
serveModelTrMap = writeJSON modelTrMap

modelTrMap :: Map.Map Text Text
modelTrMap = Map.fromList
  [("case", "Case")
  ,("averageCommissioner", "AverageCommissioner")
  ,("bank", "Bank")
  ,("consultation", "Consultation")
  ,("continue", "Continue")
  ,("deliverCar", "DeliverCar")
  ,("deliverParts", "DeliverParts")
  ,("hotel", "Hotel")
  ,("information", "Information")
  ,("ken", "LegalAssistance")
  ,("rent", "Rent")
  ,("sober", "SoberDriver")
  ,("taxi", "Taxi")
  ,("tech", "Tech")
  ,("tech1", "TechInspect")
  ,("tickets", "Tickets")
  ,("towage", "Towage")
  ,("transportation", "Transportation")
  ]

serveModel :: HasAuth b => Handler b (SiteConfig b) ()
serveModel = do
  Just name  <- fmap T.decodeUtf8 <$> getParam "name"
  view  <- T.decodeUtf8 . fromMaybe "" <$> getParam "view"
  model <- case T.splitOn ":" view of
      ["ctr",scr,pgm]
        | Just name' <- Map.lookup name modelTrMap
          -> case Model.dispatch name' $ viewForModel scr of
            Just (Just res)
              -> Just . setModelName name
              <$> constructModel name' scr pgm res
            _ -> error $ "Unexpected model name" ++ show name'
      _ -> case Model.dispatch name $ viewForModel view of
        Just res -> return res
        Nothing
          | Just name' <- Map.lookup name modelTrMap
          -> case Model.dispatch name' $ viewForModel view of
            Just res -> return $ setModelName name <$> res
            Nothing  -> Map.lookup name <$> gets models
        _ -> Map.lookup name <$> gets models

  mcu   <- withAuth currentUser
  case return (,) `ap` mcu `ap` model of
    Nothing -> finishWithError 401 ""
    Just (cu, m) ->
      case view `elem` ["search", "portalSearch"] of
        True  -> writeModel m
        False -> stripModel cu m >>= writeModel


viewForModel :: forall m . Model.Model m => T.Text -> m -> Maybe Model
viewForModel name _
  = join
  $ fmap (Aeson.decode . Aeson.encode)
  $ (Model.modelView name :: Maybe (Model.ModelView m))


setModelName :: Text -> Model -> Model
setModelName n m = m {modelName = T.encodeUtf8 n}

constructModel
  :: Text -> Text -> Text -> Model
  -> Handler b (SiteConfig b) Model
constructModel mdlName screen program model = do
  let q = [sql|
      select c.field, c.label, c.r, c.w, c.required, c.info, c.ord
        from "ConstructorFieldOption" c, "CtrModel" m, "CtrScreen" s
        where m.id = c.model and s.id = c.screen
          and m.value = ?
          and s.value = ?
          and program = ? :: int
        order by ord asc
      |]
  pg <- gets pg_search
  res <- liftIO (withResource pg $ \c -> query c q [mdlName,screen,program])
  let optMap = Map.fromList [(nm,(l,r,w,rq,inf,o)) | (nm,l,r,w,rq,inf,o) <- res]
  let adjustField f = case Map.lookup (name f) optMap of
        Nothing -> [f] -- NB: field is not modified if no options found
        Just (_,False,_,_,_,_) -> [] -- unreadable field
        Just (l,True,w,rq,inf,o) ->
          [f {meta
              = Map.insert "label"    (Aeson.String l)
              . Map.insert "required" (Aeson.Bool rq)
              . Map.insert "infoText" (Aeson.String inf)
              . Map.insert "readonly" (Aeson.Bool $ not w)
              <$> meta f
            , sortingOrder = Just o
            , canWrite = w}
          ]
  return $ model
    {fields = sortBy (comparing sortingOrder)
        $ concatMap adjustField
        $ fields model
    }


writeModel :: Model -> Handler b (SiteConfig b) ()
writeModel model
  = writeJSON
  =<< case modelName model of
    -- If sid parameter is specified for Contract model, serve model
    -- for portal screen table/form.
    "Contract" -> do
      sid <- getParam "sid"
      case sid of
        Just i -> do
          field <- fromMaybe "showform" <$> getParam "field"
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
      -- Use Case permissions even when faked to serve Case model
      -- while being asked for case (see oldCRUD branch in
      -- serveModel).
      fixCaseModelName "case" = "Case"
      fixCaseModelName v      = v
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
    (unUid uid, fixCaseModelName $ modelName m)
  let fieldsMap = Map.fromList readableFields
  let fieldFilter f fs = case Map.lookup (name f) fieldsMap of
        Nothing -> fs
        Just wr -> f {canWrite = canWrite f && wr} : fs
  return $ m {fields = foldr fieldFilter [] $ fields m}

-- | Serve available idents for a model (given in @name@ request
-- parameter) as JSON object: @{"foo": 12, "bar": 28}@.
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

  programInfos <- withPG
    $ selectJSON (ProgramInfo.program :. ProgramInfo.info)
  serviceInfos <- withPG
    $ selectJSON (ServiceInfo.program :. ServiceInfo.service :. ServiceInfo.info)
  serviceNames <- withPG
    $ selectJSON (ServiceNames.ident :. ServiceNames.value :. ServiceNames.label :. ServiceNames.icon)

  Aeson.Object dictMap <- gets dictionaries
  -- Support legacy client interface for some dictionaries
  writeJSON $ Aeson.Object
    $ HM.insert "ProgramInfo"
      (Aeson.object [("entries", Aeson.Array $ V.fromList programInfos)])
    $ HM.insert "ServiceInfo"
      (Aeson.object [("entries", Aeson.Array $ V.fromList serviceInfos)])
    $ HM.insert "ServiceNames"
      (Aeson.object [("entries", Aeson.Array $ V.fromList serviceNames)])
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
      , ("modelTrMap",   method GET serveModelTrMap)
      ]
    liftIO $ SiteConfig
      <$> loadModels cfgDir
      <*> loadDictionaries cfgDir
      <*> pure pg_pool
