{-# LANGUAGE QuasiQuotes #-}

{-|

Postgres-based roles storage for Snap authentication system.

Roles are stored in @snap_auth_user_roles@ table with the following
schema:

> CREATE TABLE snap_auth_user_roles (uid INTEGER references snap_auth_user(uid),
>                                   role TEXT);

-}

module Snaplet.Auth.PGRoles
    ( userRolesPG
    , replaceRolesFromPG
    )
where

import Database.PostgreSQL.Simple.SqlQQ

import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.PostgresqlSimple


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
-- | Replace roles for a user with those stored in Postgres.
replaceRolesFromPG :: HasPostgres m => AuthUser -> m AuthUser
replaceRolesFromPG user =
    userRolesPG user >>= \roles -> return user{userRoles = roles}
