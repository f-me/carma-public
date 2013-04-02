{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Postgres-based roles & user meta storage for Snap authentication
system.

Roles are stored in @usermetatbl@ table with the following schema:

> CREATE TABLE snap_auth_user_roles (uid INTEGER references snap_auth_user(uid),
>                                    role TEXT[],
>                                    realName TEXT[]);

-}

module Snaplet.Auth.PGUsers
    ( -- * User roles 
      userRolesPG
    , replaceRolesFromPG
      -- * List of all users
    , UsersList(..)
    , usersListPG
    )
where

import Data.Aeson.TH
import Data.ByteString.Char8 (ByteString)
import Data.Map as M (Map, empty, insert)

import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.SqlQQ

import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.PostgresqlSimple
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



type UserEntry = M.Map ByteString ByteString


data UsersList = UsersList [UserEntry]
                 deriving (Show)

$(deriveToJSON id ''UsersList)


------------------------------------------------------------------------------
-- | Select all user logins together with comma-separated lists of
-- attached roles.
usersListQuery :: Query
usersListQuery = [sql|
SELECT login,string_agg(role,',') 
FROM snap_auth_user u, snap_auth_user_roles r
WHERE r.uid=u.uid group by u.uid;
|]


------------------------------------------------------------------------------
-- | Get list of all users from the database.
usersListPG :: HasPostgres m => m UsersList
usersListPG = do
    rows <- query_ usersListQuery
    let dic = map
              (\(login, roles) -> 
                   M.insert "roles" roles $
                   M.insert "value" login $
                   M.empty)
              rows
    return $ UsersList dic
