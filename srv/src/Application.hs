{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Top-level application with AuthManager, Redson and utility handlers.
Serve index page, check user login.

-}

module Application (appInit)

where

import Prelude hiding (lookup)

import Control.Monad.IO.Class
import Data.Functor
import Data.Maybe

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BU (toString)
import Data.Configurator
import Data.Lens.Template
import Data.Time.Clock

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe

import Web.ClientSession

import Snap.Snaplet.Redson


------------------------------------------------------------------------------
-- | Application snaplet state type: Redson, Heist.
data App = App
    { _heist :: Snaplet (Heist App)
    , _redson :: Snaplet (Redson App)
    , _session :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _startTime :: UTCTime
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
-- | Render empty form for model.
indexPage :: AppHandler ()
indexPage = ifTop $ render "index"


------------------------------------------------------------------------------
-- | Redirect using 303 See Other to login form.
--
-- Used after unsuccessful access/login attempt or logout.
redirectToLogin :: MonadSnap m => m a
redirectToLogin = redirect' "/login/" 303


------------------------------------------------------------------------------
-- | If user is not logged in, redirect to login page, pass to
-- handler otherwise.
authOrLogin :: AppHandler () -> AppHandler ()
authOrLogin h = requireUser auth redirectToLogin h


------------------------------------------------------------------------------
-- | Render empty login form.
loginForm :: AppHandler ()
loginForm = do
  serveFile $ "resources/templates/login.html"


------------------------------------------------------------------------------
-- | Login user.
doLogin :: AppHandler ()
doLogin = ifTop $ do
  l <- fromMaybe "" <$> getParam "login"
  p <- fromMaybe "" <$> getParam "password"
  r <- maybe False (const True) <$> getParam "remember"
  res <- with auth $ loginByUsername l (ClearText p) r
  case res of
    Left err -> redirectToLogin
    Right user -> redirect "/"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/", method GET $ authOrLogin indexPage)
         , ("/login/", method GET loginForm)
         , ("/login/", method POST doLogin)
         , ("/logout/", with auth $ logout >> redirectToLogin)
         , ("/s/", serveDirectory "resources/static")
         ]


sessionTimeout :: Maybe Int
sessionTimeout = Nothing


------------------------------------------------------------------------------
-- | The application initializer.
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Forms application" Nothing $ do
  r <- nestSnaplet "_" redson $ redsonInit auth

  h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
  addAuthSplices auth

  cfg <- getSnapletUserConfig
  sesKey <- liftIO $
            lookupDefault "resources/private/client_session_key.aes"
                          cfg "session-key"
  rmbKey <- liftIO $
            lookupDefault "resources/private/site_key.txt"
                          cfg "remember-key"
  rmbPer <- liftIO $
            lookupDefault 14 
                          cfg "remember-period"
  authDb <- liftIO $
            lookupDefault "resources/private/users.json"
                          cfg "user-db"

  s <- nestSnaplet "session" session $
       initCookieSessionManager sesKey "_session" sessionTimeout
  a <- nestSnaplet "auth" auth $
       initJsonFileAuthManager
       defAuthSettings{ asSiteKey = rmbKey
                      , asRememberPeriod = Just (rmbPer * 24 * 60 * 60)}
                               session authDb

  sTime <- liftIO getCurrentTime

  addRoutes routes

  return $ App h r s a sTime

-- {- Unported code. -}
--
-- searchCase = do
--   let response = A.encode $ object
--         ["iTotalRecords" .= (0::Int)
--         ,"iTotalDisplayRecords" .= (0::Int)
--         ,"aaData" .= A.toJSON ([]::[T.Text])
--         ]
--   modifyResponse $ setContentType "application/json"
--   writeLBS response


-- searchDealer = search "dealer"
--   ["name","city","program","salesAddr", "salesPhone"]
--   [("name","sSearch_0")
--     ,("city","sSearch_1")
--     ,("program","sSearch_2")]


-- searchContractor = search "partner"
--   ["companyName","cityRu","contactPerson","contactPhone","serviceRu"]
--   [("companyName","sSearch_0")
--     ,("contactPerson","sSearch_2")
--     ,("contactPhone","sSearch_3")]


-- search keyPrefix outFields searchFields = do
--   let (attrs,pats) = unzip searchFields

--   ps <- rqParams <$> getRequest
--   let si = map (head . (M.!) ps) pats
--   let displayStart = head $ (M.!) ps "iDisplayStart"

--   (vals,total) <- runRedisDB redisDB $ do
--     let searchKeys = catMaybes $ zipWith
--           (\k s -> if B.null s
--             then Nothing
--             else Just $ B.concat [keyPrefix,":",k,":*",s,"*"])
--           attrs si

--     matchingKeys <- if null searchKeys
--         then (:[]). fromRight <$> keys (B.concat [keyPrefix,":*"])
--         else rights <$> mapM keys searchKeys
    
--     keys' <- foldl1' intersect . map (foldl' union [])
--           <$> forM matchingKeys
--             ((rights <$>) . mapM (\k ->lrange k 0 (-1)))

--     vals  <- (catMaybes . fromRight) <$>  mget (take 100 keys')
--     return (vals, length keys')

--   let res = catMaybes $ flip map
--         (catMaybes $ map (A.decode . L.fromChunks .(:[])) vals)
--         $ A.parseMaybe (\o -> mapM (o .:) outFields)
--         :: [[A.Value]]

--   let response = A.encode $ object
--         ["iTotalRecords" .= total
--         ,"iTotalDisplayRecords" .= (100::Int)
--         ,"aaData" .= toJSON res
--         ]
--   modifyResponse $ setContentType "application/json"
--   writeLBS response


-- fromRight = either (const []) id
