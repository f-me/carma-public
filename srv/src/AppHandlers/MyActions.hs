
module AppHandlers.MyActions where

import Control.Monad
import Data.Functor
import Data.String (fromString)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import Data.Aeson as Aeson


import Snap
import Snap.Snaplet
import Snap.Snaplet.Auth
import qualified Snaplet.DbLayer as DB
import Database.PostgreSQL.Simple
----------------------------------------------------------------------
import Application
import AppHandlers.Util


{-
selectActions :: AppHandler [Map ByteString ByteString]
selectActions = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString $
    "select id::text, assignedTo, "
    ++ "(extract (epoch from dueTime)::int)::text, "
    ++ "garbage::hstore -> 'priority', garbage::hstore -> 'targetGroup' "
    ++ "from actiontbl where "
    ++ "extract (epoch from dueTime) > 0 "
    ++ "and closed = false"
  let fields = ["id", "assignedTo", "duetime", "priority", "targetGroup"]
  return $ map (Map.fromList . zip fields . map (fromMaybe "")) rows
-}

allActionsHandler :: AppHandler ()
allActionsHandler = undefined


assignMeAnAction :: AppHandler ()
assignMeAnAction = do
  Just cUsr <- with auth currentUser
  let uLogin = T.unpack $ userLogin cUsr
  let uRole  = T.unpack . head $ userRoles cUsr
  logdUsers' <- addToLoggedUsers cUsr
  let logdUsersList = T.intercalate "','" $ map snd $ Map.elems logdUsers

  actIds <- withPG pg_search $ \c -> query_ c $ fromString
    $  "UPDATE actiontbl SET assignedTo = '" ++ uLogin ++ "'"
    ++ "  WHERE id = (SELECT id FROM actiontbl"
    ++ "    WHERE closed = false"
    ++ "    AND   duetime - now() < interval '30 minutes'"
    ++ "    AND   targetGroup = '" ++ uRole ++ "'"
    ++ "    AND   (assignedTo IS NULL"
    ++ "           OR assignedTo NOT IN ('" ++ logdUsersList ++ "'))"
    ++ "    ORDER BY abs (extract (epoch from duetime - now())::integer) ASC,"
    ++ "             priority ASC"
    ++ "    LIMIT 1)"
    ++ "  RETURNING id;"

  with db $ forM_ actIds $ \actId->
      DB.update "action" actId
        $ Map.singleton "assignedTo" $ T.encodeUtf8 uLogin


myActionsHandler :: AppHandler ()
myActionsHandler = undefined
