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

import Control.Lens (_1, view)
import Control.Monad
import Data.Aeson as A
import Data.Maybe

import Database.PostgreSQL.Simple.SqlQQ
import Snap.Snaplet.PostgresqlSimple

import Data.Model.Types

import Carma.Model.ActionType as ActionType
import Carma.Model.UrgentServiceReason as USR

import Snaplet.Auth.PGUsers

import Application
import AppHandlers.Backoffice
import AppHandlers.Users
import Util


topPriority :: Int
topPriority = 1


leastPriority :: Int
leastPriority = 5


-- | Assign a single action to a user, yield action id, case id and call id.
--
-- 3 parameters: current usermeta ident, UrgentServiceReason.notUrgent
-- ident, order action types (as array for IN clause)
assignQ :: Query
assignQ = [sql|
      WITH
      currentUser AS (SELECT * FROM usermetatbl WHERE id = ?),
      notUrgent AS (SELECT * FROM "UrgentServiceReason" WHERE id = ?),
      activeUsers AS (
        SELECT s.userId as id
        FROM (SELECT DISTINCT on (userId) userId, state
            FROM "UserState"
            WHERE ctime > now() - interval '24 hours'
            ORDER BY userId, id DESC) s
        WHERE s.state <> 'LoggedOut'),
      pullableActions AS (
        SELECT actiontbl.* FROM actiontbl, currentUser u, "ActionType" t
        WHERE result IS NULL
        AND actiontbl.type = t.id
        AND t.priority > 0
        AND targetGroup = ANY (u.roles)
        AND (assignedTo IS NULL
             OR assignedTo NOT IN (SELECT id FROM activeUsers))
        AND duetime <= now() + interval '5 minutes'
        FOR UPDATE of actiontbl)
      UPDATE actiontbl SET assignTime = now(), assignedTo = u.id
        FROM currentUser u
        WHERE actiontbl.id = (SELECT act.id
          FROM (pullableActions act
            LEFT JOIN "ActionType" t ON   t.id = act.type
            LEFT JOIN servicetbl svc ON svc.id = act.serviceId
            LEFT JOIN casetbl      c ON   c.id = act.caseId),
            currentUser u,
            notUrgent
          WHERE
              (coalesce(
                  array_length(u.boPrograms, 1),
                  array_length(u.boCities, 1)) is null
               OR (c.program::text = ANY (u.boPrograms) OR c.city = ANY (u.boCities)))
          ORDER BY
            priority ASC,
            (u.boPrograms IS NOT NULL AND c.program::text = ANY (u.boPrograms)) DESC,
            (u.boCities   IS NOT NULL AND c.city          = ANY (u.boCities)) DESC,
            (act.type IN ?
              AND coalesce(svc.urgentService, notUrgent.id) <> notUrgent.id) DESC,
            act.duetime ASC
          LIMIT 1)
        RETURNING actiontbl.id, caseId, callId;
    |]


-- | Try to pull a new action and serve JSON with all actions
-- currently assigned to the user. No action is pulled if the
-- user is not Ready or has older due actions.
littleMoreActionsHandler :: AppHandler ()
littleMoreActionsHandler = logExceptions "littleMoreActions" $ do
  uid <- fromMaybe (error "No current user") <$> currentUserMetaId
  let orders = In [ActionType.orderService, ActionType.orderServiceAnalyst]
      Ident uid' = uid

  -- Actions already assigned to the user
  oldActions <- map (\(Only i :. Only caseId :. Only callId :. ()) ->
                       (i, caseId, callId)) <$>
                (liftPG' (myActionsQ uid))

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
      newActions <- query assignQ (uid, USR.notUrgent, orders)

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
