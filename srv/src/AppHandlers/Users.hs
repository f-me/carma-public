{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE QuasiQuotes #-}

{-|

Combinators and helpers for user permission checking.

-}

module AppHandlers.Users
    ( chkAuth
    , chkAuthLocal
    , chkAuthPartner
    , claimActivity
    )

where

import Snap
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.PostgresqlSimple
import Database.PostgreSQL.Simple.SqlQQ

import Application
import AppHandlers.Util


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
chkAuth h = chkAuthRoles alwaysPass (claimActivity >> h)


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
-- | Select list of roles for a user with uid given as a query
-- parameter.
userRolesQuery :: Query
userRolesQuery = [sql|
SELECT role FROM snap_auth_user_roles WHERE uid=?;
|]


------------------------------------------------------------------------------
-- | Get list of roles from the database for a user.
userRolesPG :: HasPostgres m => AuthUser -> m [Role]
userRolesPG user =
    case userId user of
      Nothing -> return []
      Just (UserId uid) -> do
        rows <- query userRolesQuery (Only uid)
        return $ map (Role . head) rows


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
          uRoles <- with authDb $ userRolesPG u
          if roleCheck uRoles
          then handler
          else handleError 401)
  -- No checks for requests from localhost
  else handler


claimActivity :: AppHandler ()
claimActivity
  = with auth currentUser
  >>= maybe (return ()) (void . addToLoggedUsers)
