{-# LANGUAGE DoAndIfThenElse #-}

{-|

Combinators and helpers for user permission checking.

-}

module AppHandlers.Users
    ( chkAuth
    , chkAuthLocal
    , chkAuthPartner
    , claimUserActivity
    , claimUserLogout
    , serveUserCake
    )

where

import Data.Aeson
import qualified Data.HashMap.Strict as HM

import Snap
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.PostgresqlSimple
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ

import Application
import AppHandlers.Util
import Snaplet.Auth.PGUsers


------------------------------------------------------------------------------
-- | Users with this role are considered local (not be confused with
-- users from localhost).
localRole :: Role
localRole = Role "local"


partnerRole :: Role
partnerRole = Role "partner"


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated users.
chkAuth :: AppHandler () -> AppHandler ()
chkAuth h = chkAuthRoles alwaysPass h


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated or non-local users.
chkAuthLocal :: AppHandler () -> AppHandler ()
chkAuthLocal f = chkAuthRoles (hasAnyOfRoles [localRole]) f


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated or non-partner users.
--
-- Auth checker for partner screens
chkAuthPartner :: AppHandler () -> AppHandler ()
chkAuthPartner f =
  chkAuthRoles (hasAnyOfRoles [partnerRole, Role "head", Role "supervisor"]) f


------------------------------------------------------------------------------
-- | A predicate for a list of user roles.
type RoleChecker = [Role] -> Bool


------------------------------------------------------------------------------
-- | Produce a predicate which matches any list of roles
alwaysPass :: RoleChecker
alwaysPass = const True


hasAnyOfRoles :: [Role] -> RoleChecker
hasAnyOfRoles authRoles =
    \userRoles -> any (flip elem authRoles) userRoles


hasNoneOfRoles :: [Role] -> RoleChecker
hasNoneOfRoles authRoles =
    \userRoles -> not $ any (flip elem authRoles) userRoles


------------------------------------------------------------------------------
-- | Pass only requests from localhost users or non-localhost users
-- with a specific set of roles.
chkAuthRoles :: RoleChecker
             -- ^ Check succeeds if non-localhost user roles satisfy
             -- this predicate.
             -> AppHandler () -> AppHandler ()
chkAuthRoles roleCheck handler = do
  req <- getRequest
  if rqRemoteAddr req /= rqLocalAddr req
  then with auth currentUser >>= maybe
       (handleError 401)
       (\u -> do
          uRoles <- with db $ userRolesPG u
          if roleCheck uRoles
          then handler
          else handleError 401)
  -- No checks for requests from localhost
  else handler


claimUserActivity :: AppHandler ()
claimUserActivity = with auth currentUser >>= \case
  Nothing -> return ()
  Just u  -> void $ execute
    "UPDATE usermetatbl SET lastactivity = NOW() WHERE login = ?"
    [userLogin u]

claimUserLogout :: AppHandler ()
claimUserLogout = with auth currentUser >>= \case
  Nothing -> return ()
  Just u  -> void $ execute
    "UPDATE usermetatbl SET lastlogout = NOW() WHERE login = ?"
    [userLogin u]


------------------------------------------------------------------------------
-- | Serve user account data back to client.
serveUserCake :: AppHandler ()
serveUserCake
  = ifTop $ with auth currentUser
  >>= \case
    Nothing -> handleError 401
    Just u'  -> do
      u <- with db $ replaceMetaRolesFromPG u'
      [(calls,orders,actions)] <- withPG pg_search $ \c -> PG.query c [sql|
        with
          calls  as (
            select count(*) as res from calltbl
              where callTaker = ?
                and calldate > now() - interval '20 days'),
          orders as (
            select count(*) as res from actiontbl
              where closed and assignedTo = ?
                and closeTime > now() - interval '20 days'
                and name = 'orderService'),
          actions as (
            select count(*) as res from actiontbl
              where closed and assignedTo = ?
                and closeTime > now() - interval '20 days'
                and dueTime <= closeTime)
          select calls.res, orders.res, actions.res
            from calls, orders, actions
        |] (userLogin u, userLogin u, userLogin u)
      writeJSON $ u
        {userMeta = HM.insert "achievements"
          (object
            ["calls" .= (calls :: Int)
            ,"orders" .= (orders :: Int)
            ,"actions" .= (actions :: Int)
            ])
          (userMeta u)
        }
