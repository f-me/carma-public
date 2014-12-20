{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ApplicationHandlers
    (
      -- * Authentication
      indexPage
    , redirectToLogin
    , authOrLogin
    , loginForm
    , doLogin
    , doLogout

      -- * CRUD
    , createHandler
    , readHandler
    , readManyHandler
    , updateHandler

      -- * Helper handlers
    , getRegionByCity
    , towAvgTime
    , copyCtrOptions

    -- * Misc. client support handlers
    , clientConfig
    , errorsHandler
    )

-- FIXME: reexport AppHandlers/* & remove import AppHandlers.* from AppInit
where

import Data.Functor
import Control.Monad.Trans.Either

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import qualified Data.Aeson as Aeson
import Data.Aeson
import qualified Data.Map as Map

import Data.Maybe

import Database.PostgreSQL.Simple (Query, query, execute)
import Database.PostgreSQL.Simple.SqlQQ
import qualified Snap.Snaplet.PostgresqlSimple as PS
import Data.Pool (withResource)
import Heist
import Heist.Interpreted
import Text.XmlHtml as X

import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Util.FileServe (serveFile)
import Snap.Util.FileUploads (getMaximumFormInputSize)

import Snaplet.FileUpload (FileUpload(cfg))

import Carma.Model
import Data.Model.CRUD
import Data.Model.Patch as Patch (Patch(..), differenceFrom)
import Carma.Model.Event (EventType(..))

import Application
import AppHandlers.Util
import Util as U hiding (render, withPG)
import Utils.LegacyModel (readIdent)

import Utils.Events (logLogin, logCRUD, updateUserState)

import ModelTriggers


------------------------------------------------------------------------------
-- | Render empty form for model.
indexPage :: AppHandler ()
indexPage = ifTop $ do
    ln <- gets $ localName . options
    -- Render index page with <addLocalName> splice defined, which
    -- appends the @local-name@ config option to its argument.
    let addLocalName :: Splice AppHandler
        addLocalName = do
            t <- X.nodeText <$> getParamNode
            let r = case ln of
                      Just s  -> T.concat [t, " [", s, "]"]
                      Nothing -> t
            return [X.TextNode r]
        splices = "addLocalName" ## addLocalName
    renderWithSplices "index" splices


------------------------------------------------------------------------------
-- | Redirect using 303 See Other to login form.
--
-- Used after unsuccessful access/login attempt or logout.
redirectToLogin :: MonadSnap m => m a
redirectToLogin = redirect' "/login" 303


------------------------------------------------------------------------------
-- | If user is not logged in, redirect to login page, pass to
-- handler otherwise.
authOrLogin :: AppHandler () -> AppHandler ()
authOrLogin = requireUser auth redirectToLogin


------------------------------------------------------------------------------
-- | Render empty login form.
loginForm :: AppHandler ()
loginForm = serveFile "resources/static/tpl/login.html"


------------------------------------------------------------------------------
-- | Login user.
doLogin :: AppHandler ()
doLogin = ifTop $ do
  l <- fromMaybe "" <$> getParam "login"
  p <- fromMaybe "" <$> getParam "password"
  r <- isJust <$> getParam "remember"
  res <- with auth $ loginByUsername (T.decodeUtf8 l) (ClearText p) r
  case res of
    Left  _ -> redirectToLogin
    Right _ -> do
      logLogin Login
      redirect "/"


doLogout :: AppHandler ()
doLogout = ifTop $ do
  logLogin Logout
  with auth logout
  redirectToLogin


------------------------------------------------------------------------------
-- | CRUD

-- FIXME: this is way too slow
readInt :: (Read i, Integral i) => ByteString -> i
readInt = read . read . show


createHandler :: AppHandler ()
createHandler = do
  Just model <- getParamT "model"
  let createModel :: forall m . Model m => m -> AppHandler Aeson.Value
      createModel _ = do
        commit <- getJSONBody :: AppHandler (Patch m)
        runCreateTriggers commit >>= \case
          Left err -> error $ "in createHandler: " ++ show err
          Right (idt@(Ident i), commit') -> do
            -- Can't do this in trigger because it need ident
            evIdt <- logCRUD Create idt commit
            updateUserState Create idt commit evIdt
            -- we really need to separate idents from models
            -- (so we can @Patch.set ident i commit@)
            return $ case Aeson.toJSON (commit' `Patch.differenceFrom` commit) of
              Object obj
                -> Object
                $ HM.insert "id" (Aeson.Number $ fromIntegral i) obj
              obj -> error $ "impossible: " ++ show obj
  void $ logResp $
    fromMaybe (error "Unknown model") $ Carma.Model.dispatch model createModel


readHandler :: AppHandler ()
readHandler = do
  Just model <- getParamT "model"
  Just objId <- getParamT "id"
  let readModel :: forall m . Model m => m -> AppHandler ()
      readModel _ = do
        res <- do
          let ident = readIdent objId :: IdentI m
          s <- PS.getPostgresState
          liftIO $ withResource (PS.pgPool s)
                     (runEitherT . crud_read getModelCRUD ident)
        case res of
          Right obj              -> writeJSON obj
          Left (NoSuchObject _)  -> handleError 404
          Left err               -> error $ "in readHandler: " ++ show err
  fromMaybe (error "Unknown model") $ Carma.Model.dispatch model readModel


readManyHandler :: AppHandler ()
readManyHandler = do
  Just model  <- getParamT "mdl" -- NB: this param can shadow query params
  limit  <- maybe 2000 readInt <$> getParam "limit"
  offset <- maybe    0 readInt <$> getParam "offset"
  params <- getQueryParams
  let queryFilter =
          [(T.decodeUtf8 k, T.decodeUtf8 v)
          | (k,v:_) <- Map.toList params
          , not $ k `elem` ["limit", "offset"]
          ]
  let readModel :: forall m . Model m => m -> AppHandler ()
      readModel _ = do
        res <- do
          s   <- PS.getPostgresState
          liftIO $ withResource
            (PS.pgPool s)
            (runEitherT . crud_readManyWithFilter
                        (getModelCRUD :: CRUD m) limit offset queryFilter)
        case res of
          Right obj -> writeJSON obj
          Left err  -> error $ "in readHandler: " ++ show err
  fromMaybe (error "Unknown model") $ Carma.Model.dispatch model readModel


updateHandler :: AppHandler ()
updateHandler = do
  Just model <- getParamT "model"
  Just objId <- getParamT "id"
  let updateModel :: forall m. Model m =>
                     m -> AppHandler (Either Int Aeson.Value)
      updateModel _ = do
        let ident = readIdent objId :: IdentI m
            recode x = case (Aeson.decode $ Aeson.encode x) of
                         Just obj -> Right obj
                         err      -> error $
                                     "BUG in updateHandler: " ++ show err
        commit <- getJSONBody :: AppHandler (Patch m)
        logReq commit
        runUpdateTriggers  ident commit >>= \case
          Left err -> error $ "in updateHandler: " ++ show err
          Right commit' -> do
            evIdt <- logCRUD Update ident commit
            updateUserState Update ident commit evIdt
            return $ recode (commit' `Patch.differenceFrom` commit)
  fromMaybe (error "Unknown model") (Carma.Model.dispatch model updateModel) >>=
    \case
      Left n -> handleError n
      Right o -> logResp $ return o


-- | Calculate average tower arrival time (in seconds) for today,
-- parametrized by city (a key from City dictionary).
towAvgTimeQuery :: Query
towAvgTimeQuery = [sql|
WITH towtimes AS (
 SELECT max(t.times_factServiceStart - a.ctime)
 FROM actiontbl a, casetbl c, towagetbl t
 WHERE a.serviceId = t.id
 AND a.caseid = c.id
 AND a.type=1
 AND c.city=?
 AND (CURRENT_DATE, INTERVAL '1 day') OVERLAPS (c.callDate, c.callDate)
 GROUP BY a.serviceId)
SELECT extract(epoch from avg(max)) FROM towtimes;
|]

-- | Read city name from @city@ request parameter and return results
-- of 'towAvgTime' query for that city as a single-element JSON list
-- (possibly containing @null@ if the time cannot be calculated).
towAvgTime :: AppHandler ()
towAvgTime = do
  city <- getIntParam "city"
  case city of
    Just c -> do
          rows <- withPG pg_search $
                  \conn -> query conn towAvgTimeQuery [c]
          writeJSON (map head rows :: [Maybe Double])
    _ -> error "Could not read city from request"


getRegionByCity :: AppHandler ()
getRegionByCity =
  getParam "city" >>= \case
    Just city -> do
      res <- withPG pg_search $ \c -> query c
        [sql|
          SELECT r.label
          FROM "Region" r, "City" c
          WHERE c.id = ANY(r.cities) AND c.id = ?
        |]
        [city]
      writeJSON (res :: [[Text]])
    _ -> error "Could not read city from request"


-- | Serve parts of the application config to client in JSON.
clientConfig :: AppHandler ()
clientConfig = do
  mus <- with fileUpload $ gets (fromIntegral . getMaximumFormInputSize . cfg)
  opts <- gets options
  let config :: Map.Map T.Text Aeson.Value
      config =
        Map.fromList
        [ ("max-file-size", Aeson.Number mus)
        , ("csta-ws-host", maybe Aeson.Null Aeson.String $ cstaWsHost opts)
        , ("csta-ws-port",
           Aeson.Number $ fromIntegral $ cstaWsPort opts)
        ]
  writeJSON config


errorsHandler :: AppHandler ()
errorsHandler = do
  r  <- readRequestBody 4096
  ip <- rqRemoteAddr <$> getRequest
  user <- fmap userLogin <$> with auth currentUser
  syslogJSON Warning "handler/errorsHandler"
    ["err" .= TL.decodeUtf8 r
    ,"ip"  .= T.decodeUtf8 ip
    ,"user".= user
    ]
  writeJSON ()


copyCtrOptions :: AppHandler ()
copyCtrOptions = do
  from <- getParam "from"
  to   <- getParam "to"
  withPG pg_search $ \c -> do
    void $ execute c
      [sql|delete from "ConstructorFieldOption" where program = ?|]
      [to]
    void $ execute c
      [sql|
        insert into "ConstructorFieldOption"
            (model,screen,program,ord,field,label,info,required,r,w)
          select model,screen,?::int,ord,field,label,info,required,r,w
            from "ConstructorFieldOption"
            where program = ?
      |]
      [to, from]
  writeJSON ()

logReq :: Aeson.ToJSON v => v -> AppHandler ()
logReq commit  = do
  user <- fmap userLogin <$> with auth currentUser
  r <- getRequest
  syslogJSON Info "handler/logReq"
    ["ip"     .= show (rqRemoteAddr r)
    ,"user"   .= user
    ,"method" .= show (rqMethod r)
    ,"uri"    .= show (rqURI r)
    ,"params" .= show (rqParams r)
    ,"body"   .= commit
    ]

logResp :: Aeson.ToJSON v => AppHandler v -> AppHandler ()
logResp act = logExceptions "handler/logResp" $ do
  r <- act
  syslogJSON Info "handler/logResp" ["response" .= r]
  writeJSON r
