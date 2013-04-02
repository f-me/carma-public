{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{-|

Postgres-based roles & user meta storage for Snap authentication
system.

Roles are stored in @usermetatbl@ table with the following schema:

> CREATE TABLE usermetatbl (uid INTEGER references snap_auth_user(uid),
>                           role TEXT[],
>                           realName TEXT,
>                           boCities TEXT,
>                           boPrograms TEXT);

-}

module Snaplet.Auth.PGUsers
    ( -- * User roles 
      userRolesPG
    , replaceRolesFromPG
      -- * User meta
    , UserMeta(..)
    , userMetaPG
      -- * List of all users
    , UsersList(..)
    , usersListPG
    )
where

import Control.Applicative
import Data.Functor
import Control.Monad

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Char8 (ByteString, intercalate)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Map as M (Map, empty, insert)
import Data.HashMap.Strict (HashMap)

import Database.PostgreSQL.Simple ((:.))
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.SqlQQ

import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.PostgresqlSimple

import qualified Data.Vector as V


-- | A rigid Haskell-only model for user meta stored in @usermetatbl@.
-- Matched by usermeta CRUD model.
--
-- boCities/Programs are both stored as comma-separated strings.
-- weathercities (TODO) is stored as a list.
data UserMeta = UserMeta { metaRoles  :: [Role]
                         , realName   :: Text
                         , boCities   :: Text
                         , boPrograms :: Text
                         }


instance FromRow UserMeta where
    fromRow = (field :: RowParser Int) >>
        UserMeta
        <$> field
        <*> field
        <*> field
        <*> field


instance FromField [Role] where
    fromField f dat = (map Role . V.toList) <$> fromField f dat


------------------------------------------------------------------------------
-- | Select roles and metas for a user with uid given as a query
-- parameter.
userMetaQuery :: Query
userMetaQuery = [sql|
SELECT * FROM usermetatbl WHERE uid=?;
|]


------------------------------------------------------------------------------
-- | Select logins, roles and metas for all users.
allUsersQuery :: Query
allUsersQuery = [sql|
SELECT u.login, m.* FROM usermetatbl m, snap_auth_user u WHERE u.uid=m.uid;
|]


------------------------------------------------------------------------------
-- | Get meta from the database for a user.
userMetaPG :: HasPostgres m => AuthUser -> m (Maybe UserMeta)
userMetaPG user =
    case userId user of
      Nothing -> return Nothing
      Just (UserId uid) -> do
        rows <- query userMetaQuery (Only uid)
        return $ case rows of
          (e:_) -> Just e
          _     -> Nothing


------------------------------------------------------------------------------
-- | Get list of roles from the database for a user.
userRolesPG :: HasPostgres m => AuthUser -> m [Role]
userRolesPG user = do
    meta <- userMetaPG user
    case meta of
      Just m -> return $ metaRoles m
      Nothing -> return []


------------------------------------------------------------------------------
-- | Replace roles for a user with those stored in Postgres.
replaceRolesFromPG :: HasPostgres m => AuthUser -> m AuthUser
replaceRolesFromPG user =
    userRolesPG user >>= \roles -> return user{userRoles = roles}


-- | UserEntry contains keys: @value@ for login, @label@ for realName
-- meta, @roles@ for comma-separated list of user roles, all other
-- meta keys.
type UserEntry = M.Map ByteString ByteString


data UsersList = UsersList [UserEntry]
                 deriving (Show)

$(deriveToJSON id ''UsersList)


------------------------------------------------------------------------------
-- | Select all user logins together with comma-separated lists of
-- attached roles.
usersListQuery :: Query
usersListQuery = [sql|
SELECT login,roles
FROM snap_auth_user u, usermetatbl m
WHERE r.uid=m.uid;
|]


------------------------------------------------------------------------------
-- | Get list of all users from the database.
usersListPG :: HasPostgres m => m UsersList
usersListPG = do
  rows <- query_ allUsersQuery
  return $ UsersList $ map toEntry rows
      where
        toEntry :: ([Text] :. UserMeta) -> UserEntry
        toEntry ((login:[]) :. meta) = 
            (M.insert "value" $ encodeUtf8 login) $
            (M.insert "label" $ encodeUtf8 $ realName meta) $
            (M.insert "roles" $ 
              intercalate "," $
              map (\(Role r) -> r) $ metaRoles meta) $
            M.empty
