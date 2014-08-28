{-|

Action assignment for back office screen.

-}

module AppHandlers.ActionAssignment where

import Control.Monad

import Snap
import Snap.Snaplet.Auth
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Data.Model.Types

import Carma.Model.Usermeta as Usermeta

import Snaplet.Auth.PGUsers

import Application
import AppHandlers.CustomSearches
import AppHandlers.Util
import Util hiding (withPG)


-- 3 parameters: usermeta ident (2), action priority class
assignQ :: Query
assignQ = [sql|
      WITH activeUsers AS (
        SELECT id
        FROM usermetatbl
        WHERE (lastlogout IS NULL OR lastlogout < lastactivity)
          AND now() - lastactivity < '30 min')
      UPDATE actiontbl SET assignTime = now(), assignedTo = ?
        WHERE id = (SELECT act.id
          FROM ((SELECT * FROM actiontbl WHERE result IS NOT NULL) act
            LEFT JOIN servicetbl svc
            ON svc.type = act.serviceType and svc.id = act.serviceId),
            casetbl c, usermetatbl u, \"ActionType\" at
          WHERE u.id = ?
          AND c.id = act.caseId
          AND at.id = act.type
          AND at.priority = ?
          AND act.duetime at time zone 'UTC' - now() <= interval '5 minutes'
          AND targetGroup::int = ANY (u.roles)
          AND (act.assignedTo IS NULL
               OR act.assignedTo NOT IN (SELECT id FROM activeUsers))
          AND (coalesce(
                  array_length(u.boPrograms, 1),
                  array_length(u.boCities, 1)) is null
               OR (c.program::text = ANY (u.boPrograms) OR c.city = ANY (u.boCities)))
          ORDER BY
            (u.boPrograms IS NOT NULL AND c.program::text = ANY (u.boPrograms)) DESC,
            (u.boCities   IS NOT NULL AND c.city          = ANY (u.boCities)) DESC,
            (act.name IN ('orderService', 'orderServiceAnalyst')
              AND coalesce(svc.urgentService, 'notUrgent') <> 'notUrgent') DESC,
            (CASE WHEN act.name IN ('orderService', 'orderServiceAnalyst')
              THEN coalesce(svc.times_expectedServiceStart,act.duetime)
              ELSE act.duetime
              END) ASC
          LIMIT 1
          FOR UPDATE OF act)
        RETURNING id;
    |]

littleMoreActionsHandler :: AppHandler ()
littleMoreActionsHandler = logExceptions "littleMoreActions" $ do
  Just cUsr' <- with auth currentUser
  Just cid <- with db $ userMetaIdent cUsr'
  let params :: Int -> (IdentI Usermeta, IdentI Usermeta, Int)
      params n = (cid, cid, n)

  actIds'   <- withPG pg_actass (\c -> query c assignQ (params 1))
  actIds''  <- case actIds' of
                 []  -> withPG pg_actass (\c -> query c assignQ (params 2))
                 _   -> return actIds'
  actIds''' <- case actIds'' of
                 []  -> withPG pg_actass (\c -> query c assignQ (params 3))
                 _   -> return actIds''

  unless (null (actIds''' :: [Only Int]))
    $ syslogJSON Info "littleMoreActions" [ "login" .= show cid
                                          , "actions" .= show actIds''']

  selectActions (Just "0") (Just cid) Nothing Nothing Nothing
    >>= writeJSON
