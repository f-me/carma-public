{-|

Action assignment for back office screen.

-}

module AppHandlers.ActionAssignment (littleMoreActionsHandler)

where

import Control.Monad

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Data.Model.Types (Ident(..))
import Data.Model.Sql as Sql

import Carma.Model.Action as Action
import Carma.Model.ActionType as ActionType

import Snaplet.Auth.PGUsers

import Application
import AppHandlers.CustomSearches
import AppHandlers.Users
import AppHandlers.Util
import Util hiding (withPG)


-- | Assign a single action to a user.
--
-- 5 parameters: usermeta ident (2), action priority class, order
-- action types (2)
assignQ :: Query
assignQ = [sql|
      WITH activeUsers AS (
        SELECT u.id
        FROM "usermetatbl" u
        LEFT JOIN (SELECT DISTINCT ON (userId) state, userId
                   FROM "UserState" ORDER BY userId, id DESC) s
        ON u.id = s.userId
        WHERE s.state <> 'LoggedOut'),
      pullableActions AS (
        SELECT * FROM actiontbl
        WHERE result IS NULL
        AND (assignedTo IS NULL
             OR assignedTo NOT IN (SELECT id FROM activeUsers))
        AND duetime - now() <= interval '5 minutes'
        FOR UPDATE of actiontbl)
      UPDATE actiontbl SET assignTime = now(), assignedTo = ?
        WHERE id = (SELECT act.id
          FROM (pullableActions act
            LEFT JOIN "ActionType" t ON   t.id = act.type
            LEFT JOIN servicetbl svc ON svc.id = act.serviceId
            LEFT JOIN casetbl      c ON   c.id = act.caseId),
            usermetatbl u
          WHERE u.id = ?
          AND t.priority = ?
          AND targetGroup = ANY (u.roles)
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


-- | Try to pull a new action and serve JSON with all actions
-- currently assigned to the user. No action is pulled if the
-- user is not Ready or has older due actions.
littleMoreActionsHandler :: AppHandler ()
littleMoreActionsHandler = logExceptions "littleMoreActions" $ do
  Just cid <- currentUserMetaId
  let orders = In [ActionType.orderService, ActionType.orderServiceAnalyst]
      -- Parameters for assignQ query
      params n = (cid, cid, n :: Int, orders)
      Ident cid' = cid
      errStart = "More actions requested by user " ++ show cid'

  -- Do not pull more actions if the user already has some
  --
  -- This is not a critical error: supervisor may assign the user to
  -- actions between two polling cycles of the back office screen.
  --
  -- Note that a supervisor still may manually assign the user to
  -- actions *after* this check has fired but before serving the list
  -- of all user's actions in response.
  myActs <- withPG pg_actass $
    Sql.select $
    Action.ident :.
    Action.assignedTo `eq` (Just cid) :.
    (isNull Action.result)
  when (null myActs) $ do
    -- Reject busy users (should not be on back office screen)
    userIsReady cid >>= \v ->
      when (not v) $ error $ errStart ++ " in non-Ready state"

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

  -- Serve all due actions to client. This may include older actions
  -- or those we've just pulled.
  selectActions (Just "0") (Just cid) Nothing Nothing Nothing
    >>= writeJSON
