{-|

Action assignment for back office screen.

-}

module AppHandlers.ActionAssignment (littleMoreActionsHandler)

where

import Control.Monad

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Carma.Model.ActionType as ActionType

import Snaplet.Auth.PGUsers

import Application
import AppHandlers.CustomSearches
import AppHandlers.Util
import Util hiding (withPG)


-- | Assign a single action to a user.
--
-- 5 parameters: usermeta ident (2), action priority class, order
-- action types (2)
assignQ :: Query
assignQ = [sql|
      WITH activeUsers AS (
        SELECT id
        FROM usermetatbl
        WHERE (lastlogout IS NULL OR lastlogout < lastactivity)
          AND now() - lastactivity < '30 min'),
      act AS (
        (SELECT * FROM actiontbl WHERE result IS NULL FOR UPDATE of actiontbl))
      UPDATE actiontbl SET assignTime = now(), assignedTo = ?
        WHERE id = (SELECT act.id
          FROM (act
            LEFT JOIN servicetbl svc
            ON svc.id = act.serviceId),
            casetbl c, usermetatbl u, "ActionType" at
          WHERE u.id = ?
          AND c.id = act.caseId
          AND at.id = act.type
          AND at.priority = ?
          AND act.duetime - now() <= interval '5 minutes'
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
            (act.type IN ?
              AND coalesce(svc.urgentService, 'notUrgent') <> 'notUrgent') DESC,
            act.duetime ASC
          LIMIT 1)
        RETURNING id;
    |]


-- | Pull new actions and serve JSON with all actions currently
-- assigned to the user.
littleMoreActionsHandler :: AppHandler ()
littleMoreActionsHandler = logExceptions "littleMoreActions" $ do
  Just cid <- currentUserMetaId
  let orders = In [ActionType.orderService, ActionType.orderServiceAnalyst]
      -- Parameters for assignQ query
      params n = (cid, cid, n :: Int, orders)

  actIds'   <- withPG pg_actass (\c -> query c assignQ (params 1))
  actIds''  <- case actIds' of
                 []  -> withPG pg_actass (\c -> query c assignQ (params 2))
                 _   -> return actIds'
  actIds''' <- case actIds'' of
                 []  -> withPG pg_actass (\c -> query c assignQ (params 3))
                 _   -> return actIds''

  unless (null (actIds''' :: [Only Int]))
    $ syslogJSON Info "littleMoreActions" [ "login" .= show cid
                                          , "actions" .= show actIds'''
                                          ]

  selectActions (Just "0") (Just cid) Nothing Nothing Nothing
    >>= writeJSON
