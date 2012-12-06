
module AppHandlers.ActionAssignment where

import Control.Monad
import Control.Applicative
import Data.String (fromString)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Snap
import Snap.Snaplet.Auth
import qualified Snaplet.DbLayer as DB
import Database.PostgreSQL.Simple
----------------------------------------------------------------------
import Application
import AppHandlers.CustomSearches
import AppHandlers.Util


assignQ :: AuthUser -> [Text] -> Query
assignQ usr logdUsers = fromString
  $  "UPDATE actiontbl SET assignedTo = '" ++ uLogin ++ "'"
  ++ "  WHERE id = (SELECT id FROM actiontbl"
  ++ "    WHERE closed = false"
  ++ "    AND   duetime - now() < interval '30 minutes'"
  ++ "    AND   targetGroup = '" ++ roleStr uRole ++ "'"
  ++ "    AND   (assignedTo IS NULL"
  ++ "           OR assignedTo NOT IN ('" ++ logdUsersList ++ "'))"
  ++ "    ORDER BY abs (extract (epoch from duetime - now())::integer) ASC,"
  ++ "             priority ASC"
  ++ "    LIMIT 1)"
  ++ "  RETURNING id::text;"
  where
    uLogin = T.unpack $ userLogin usr
    uRole  = head $ userRoles usr
    roleStr (Role bs) = B.unpack bs
    logdUsersList = T.unpack $ T.intercalate "','" logdUsers


myActionsHandler :: AppHandler ()
myActionsHandler = do
  Just cUsr <- with auth currentUser
  logdUsers <- map (userLogin.snd) . Map.elems <$> addToLoggedUsers cUsr

  actIds <- withPG pg_actass (`query_` assignQ cUsr logdUsers)

  let uLogin = T.encodeUtf8 $ userLogin cUsr
  with db $ forM_ actIds $ \[actId] ->
      DB.update "action" actId
        $ Map.singleton "assignedTo" uLogin

  selectActions (Just "0") (Just uLogin) Nothing Nothing Nothing
    >>= writeJSON
