{-|

Action assignment for back office screen.

-}

module AppHandlers.ActionAssignment (littleMoreActionsHandler)

where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Aeson as A
import Data.Maybe

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Data.Model.Types
import Data.Model.Sql as Sql

import Carma.Model.Action as Action
import Carma.Model.ActionType as ActionType
import Carma.Model.Case as Case

import Snaplet.Auth.PGUsers

import Application
import AppHandlers.Users
import AppHandlers.Util
import Util hiding (withPG)


-- | Assign a single action to a user, yield action id and case id.
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
        RETURNING id, caseId;
    |]


-- | Try to pull a new action and serve JSON with all actions
-- currently assigned to the user. No action is pulled if the
-- user is not Ready or has older due actions.
littleMoreActionsHandler :: AppHandler ()
littleMoreActionsHandler = logExceptions "littleMoreActions" $ do
  uid <- fromMaybe (error "No current user") <$> currentUserMetaId
  let orders = In [ActionType.orderService, ActionType.orderServiceAnalyst]
      -- Parameters for assignQ query
      params n = (uid, uid, n :: Int, orders)
      Ident uid' = uid

  -- Actions already assigned to the user
  oldActions <- map (\(Only i :. Only caseId :. ()) -> (i, caseId)) <$>
                withPG pg_actass $
                Sql.select $
                Action.ident :. Action.caseId :.
                Action.assignedTo `eq` Just uid :.
                isNull Action.result

  actions <- ((,) <$> userIsReady uid <*> return oldActions) >>= \case
    -- Do not pull more actions if the user already has some.
    --
    -- This is not a critical error: supervisor may assign the user to
    -- actions between two polling cycles of the back office screen.
    --
    -- Note that a supervisor still may manually assign the user to
    -- actions *after* this check has fired but before the list of all
    -- user's actions is served in the response, which means that in
    -- theory more than one action may end up being assigned to one
    -- user.
    (_   , _:_) -> return oldActions
    -- Reject busy users with no actions. The business process has
    -- already been violated (should not be on back office screen).
    (False, []) -> error $
                   "More actions requested by user " ++ show uid' ++
                   " in non-Ready state"
    (True, []) -> do
      -- Pull new actions starting from top priority until something is
      -- pulled (or we run out of priorities to check). This is the
      -- opposite of Maybe monad behavior.
      let pullFurther _       []      = return []
          pullFurther puller (pr:prs) =
            puller pr >>= \case
              []  -> pullFurther puller prs
              sth -> return sth

      newActions <- pullFurther
                    (\p ->
                       withPG pg_actass (\c -> query c assignQ (params p)))
                    [1..5]

      unless (null (newActions :: [(IdentI Action, IdentI Case)])) $
       syslogJSON Info "littleMoreActions"
       [ "user" .= show uid'
       , "newActions" .= map fst newActions
       ]

      return newActions

  writeJSON $ map (\(aid, caseId) ->
                     A.object ["id" .= aid, "caseId" .= caseId]) actions
