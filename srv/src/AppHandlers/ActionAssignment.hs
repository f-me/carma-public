module AppHandlers.ActionAssignment where

import Control.Monad
import Data.String (fromString)

import qualified Data.Map as Map
import qualified Data.Text as T

import Snap
import Snap.Snaplet.Auth
import qualified Snaplet.DbLayer as DB
import Database.PostgreSQL.Simple
----------------------------------------------------------------------
import Application
import AppHandlers.CustomSearches
import AppHandlers.Util
import Util hiding (withPG)


assignQ :: Int -> AuthUser -> Query
assignQ pri usr = fromString
  $  "WITH activeUsers AS ("
  ++ "  SELECT login"
  ++ "  FROM usermetatbl"
  ++ "  WHERE (lastlogout IS NULL OR lastlogout < lastactivity)"
  ++ "    AND now() - lastactivity < '30 min') "
  ++ "UPDATE actiontbl SET assignedTo = '" ++ uLogin ++ "'"
  ++ "  WHERE id = (SELECT act.id"
  ++ "    FROM ((SELECT * FROM actiontbl WHERE closed = false) act"
  ++ "      LEFT JOIN servicetbl svc"
  ++ "      ON svc.type || ':' || svc.id = act.parentId),"
  ++ "      casetbl c, usermetatbl u"
  ++ "    WHERE u.login = '" ++ uLogin ++ "'"
  ++ "    AND c.id::text = substring(act.caseId, ':(.*)')"
  ++ "    AND priority = '" ++ show pri ++ "'"
  ++ "    AND (act.duetime at time zone 'UTC') - (now() at time zone 'UTC') <= interval '5 minutes'"
  ++ "    AND targetGroup = ANY (u.roles)"
  ++ "    AND (act.assignedTo IS NULL"
  ++ "         OR act.assignedTo NOT IN (SELECT login FROM activeUsers))"
  ++ "    AND (coalesce("
  ++ "            array_length(u.boPrograms, 1),"
  ++ "            array_length(u.boCities, 1)) is null"
  ++ "         OR (c.program::text = ANY (u.boPrograms) OR c.city = ANY (u.boCities)))"
  ++ "    ORDER BY"
  ++ "      (u.boPrograms IS NOT NULL AND c.program::text = ANY (u.boPrograms)) DESC,"
  ++ "      (u.boCities   IS NOT NULL AND c.city          = ANY (u.boCities)) DESC,"
  ++ "      (act.name IN ('orderService', 'orderServiceAnalyst')"
  ++ "        AND coalesce(svc.urgentService, 'notUrgent') <> 'notUrgent') DESC,"
  ++ "      (CASE WHEN act.name IN ('orderService', 'orderServiceAnalyst')"
  ++ "        THEN coalesce(svc.times_expectedServiceStart,act.duetime)"
  ++ "        ELSE act.duetime"
  ++ "        END) ASC"
  ++ "    LIMIT 1"
  ++ "    FOR UPDATE OF act)"
  ++ "  RETURNING id::text;"
  where
    uLogin = T.unpack $ userLogin usr


littleMoreActionsHandler :: AppHandler ()
littleMoreActionsHandler = logExceptions "littleMoreActions" $ do
  Just cUsr' <- with auth currentUser

  actIds'   <- withPG pg_actass (`query_` assignQ 1 cUsr')
  actIds''  <- case actIds' of
                 []  -> withPG pg_actass (`query_` assignQ 2 cUsr')
                 _   -> return actIds'
  actIds''' <- case actIds'' of
                 []  -> withPG pg_actass (`query_` assignQ 3 cUsr')
                 _   -> return actIds''

  let uLogin = userLogin cUsr'
  now <- liftIO $ projNow id
  with db $ forM_ actIds''' $ \[actId] ->
      DB.update "action" actId
        $ Map.fromList [("assignedTo", uLogin)
                       ,("assignTime", now)
                       ]

  when (not $ null actIds''')
    $ syslogJSON Info "littleMoreActions" ["login" .= uLogin, "actions" .= show actIds''']

  selectActions (Just "0") (Just uLogin) Nothing Nothing Nothing
    >>= writeJSON
