{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Postgres-based list of all users.

See 'Snaplet.Auth.PGRoles' for notes on user roles table schema.

-}

module Snaplet.Auth.PGUsersDict
    ( UsersList(..) 
    , usersListPG
    )
where

import Data.Aeson.TH
import Data.Map as M (Map, empty, insert)
import Data.ByteString.Char8 (ByteString)
import Database.PostgreSQL.Simple.SqlQQ

import Snap.Snaplet.PostgresqlSimple


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
