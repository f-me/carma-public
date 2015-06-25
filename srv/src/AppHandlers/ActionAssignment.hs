{-|

Action assignment for back office screen.

-}

module AppHandlers.ActionAssignment
    (
      littleMoreActionsHandler

      -- * Assignment options
    , topPriority
    , leastPriority
    )

where

import Control.Applicative hiding (some)
import Control.Lens (_1, view)
import Control.Monad
import Data.Aeson as A
import Data.Maybe

import Database.PostgreSQL.Simple.SqlQQ
import Snap.Snaplet.PostgresqlSimple

import Data.Model.Types
import Data.Model.Sql as Sql

import Carma.Model.Action as Action
import Carma.Model.ActionType as ActionType

import Snaplet.Auth.PGUsers

import Application
import AppHandlers.Users
import AppHandlers.Util
import Util


topPriority :: Int
topPriority = 1


leastPriority :: Int
leastPriority = 5


-- | Assign a single action to a user, yield action id, case id and call id.
--
-- 5 parameters: usermeta ident (3), action priority class, order
-- action types (as array for IN clause)
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
        SELECT actiontbl.* FROM actiontbl, usermetatbl u
        WHERE result IS NULL
        AND u.id = ?
        AND targetGroup = ANY (u.roles)
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
        RETURNING id, caseId, callId;
    |]


-- | Try to pull a new action and serve JSON with all actions
-- currently assigned to the user. No action is pulled if the
-- user is not Ready or has older due actions.
littleMoreActionsHandler :: AppHandler ()
littleMoreActionsHandler = logExceptions "littleMoreActions" $ do
  uid <- fromMaybe (error "No current user") <$> currentUserMetaId
  let orders = In [ActionType.orderService, ActionType.orderServiceAnalyst]
      -- Parameters for assignQ query
      params n = (uid, uid, uid, n :: Int, orders)
      Ident uid' = uid

  -- Actions already assigned to the user
  oldActions <- map (\(Only i :. Only caseId :. Only callId :. ()) ->
                       (i, caseId, callId)) <$>
                (liftPG $
                 Sql.select $
                 Action.ident :. Action.caseId :. Action.callId :.
                 Action.assignedTo `eq` Just uid :.
                 isNull Action.result)

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
                    (\p -> query assignQ (params p))
                    [topPriority..leastPriority]

      unless (null newActions) $
        syslogJSON Info "littleMoreActions"
        [ "user" .= show uid'
        , "newActions" .= map (view _1) newActions
        ]

      return newActions

  writeJSON $
    map (\(aid, caseId, callId) ->
           A.object [ "id" .= aid
                    , "caseId" .= caseId
                    , "callId" .= callId])
    actions
