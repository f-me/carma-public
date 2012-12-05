
module AppHandlers.MyActions where

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
import AppHandlers.Util

type MBS = Maybe ByteString

selectActions
  :: MBS -> MBS -> MBS -> MBS -> MBS
  -> AppHandler [Map ByteString ByteString]
selectActions mClosed mAssignee mRole mFrom mTo = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString
    $  "SELECT id::text, caseId, parentId,"
    ++ "       (closed::int)::text, name, assignedTo, targetGroup,"
    ++ "       (extract (epoch from duetime)::int)::text, "
    ++ "       result, priority"
    ++ "  FROM actiontbl WHERE true"
    ++ (maybe "" (\x -> "  AND closed = " ++ toBool x) mClosed)
    ++ (maybe "" (\x -> "  AND assignedTo = " ++ quote x) mAssignee)
    ++ (maybe "" (\x -> "  AND targetGroup = " ++ quote x) mRole)
    ++ (maybe "" (\x -> "  AND extract (epoch from duetime) >= " ++ int x) mFrom)
    ++ (maybe "" (\x -> "  AND extract (epoch from duetime) <= " ++ int x) mTo)
  let fields
        = ["id", "caseId", "parentId", "closed", "name"
          ,"assignedTo", "targetGroup", "duetime", "result"
          ,"priority"]
  return $ map (Map.fromList . zip fields . map (maybe "" id)) rows


toBool :: ByteString -> String
toBool "1" = "true"
toBool _   = "false"

quote :: ByteString -> String
quote x = "'" ++ T.unpack (T.decodeUtf8 x) ++ "'"

int :: ByteString -> String
int = T.unpack . T.decodeUtf8


allActionsHandler :: AppHandler ()
allActionsHandler
  = join (selectActions
    <$> getParam "closed"
    <*> pure Nothing
    <*> getParam "targetGroup"
    <*> getParam "duetimeFrom"
    <*> getParam "duetimeTo")
  >>= writeJSON


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

  actIds <- withPG pg_search (`query_` assignQ cUsr logdUsers)

  let uLogin = T.encodeUtf8 $ userLogin cUsr
  with db $ forM_ actIds $ \[actId] ->
      DB.update "action" actId
        $ Map.singleton "assignedTo" uLogin

  selectActions (Just "0") (Just uLogin) Nothing Nothing Nothing
    >>= writeJSON
