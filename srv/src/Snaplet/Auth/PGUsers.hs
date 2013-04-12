{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{-|

Postgres-based roles & user meta storage for Snap authentication
system.

Roles are stored in @usermetatbl@ table as created from the @usermeta@
model.

Populating user roles and meta from PG:

> u <- with auth currentUser
> u' <- with authDb $ replaceMetaRolesFromPG u

-}

module Snaplet.Auth.PGUsers
    ( -- * User roles & meta
      userRolesPG
    , UserMeta(..)
    , userMetaPG
    , replaceMetaRolesFromPG
      -- * List of all users
    , UsersList(..)
    , usersListPG
    )

where

import Control.Applicative

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Char8 (ByteString, intercalate)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Maybe
import Data.Map as M (Map, empty, insert, toList)
import Data.HashMap.Strict as HM (HashMap, fromList)

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.SqlQQ

import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.PostgresqlSimple

import qualified Data.Vector as V


-- | A rigid Haskell-only model for user meta stored in @usermetatbl@.
-- Matches a subset of @usermeta@ CRUD model. Usermeta instance id &
-- uid are ignored.
data UserMeta = UserMeta { realName      :: Maybe Text
                         , metaRoles     :: [Role]
                         , boCities      :: Maybe [ByteString]
                         , boPrograms    :: Maybe [ByteString]
                         , weatherCities :: Maybe [ByteString]
                         }


instance FromRow UserMeta where
    fromRow = (field :: RowParser Int) >> (field :: RowParser Int) >>
        UserMeta
        <$> field
        -- NULL roles is no roles
        <*> (do
              f <- field
              case f of
                Just rls -> return rls
                Nothing  -> return [])
        <*> field
        <*> field
        <*> field


instance FromField [Role] where
    fromField f dat = (map Role . V.toList) <$> fromField f dat


instance FromField [ByteString] where
    fromField f dat = V.toList <$> fromField f dat


------------------------------------------------------------------------------
-- | Select meta for a user with uid given as a query parameter.
userMetaQuery :: Query
userMetaQuery = [sql|
SELECT * FROM usermetatbl WHERE uid=?;
|]


------------------------------------------------------------------------------
-- | Select logins and metas for all users.
allUsersQuery :: Query
allUsersQuery = [sql|
SELECT u.login, m.* FROM usermetatbl m, snap_auth_user u WHERE u.uid=m.uid;
|]


------------------------------------------------------------------------------
-- | Get meta from the database for a user.
--
-- TODO Use carma-sync here, drop rigid Haskell usermeta model.
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
-- | UserEntry contains keys: @value@ for login, @label@ for realName
-- meta (or login when realName is not present), all other meta keys
-- (joining lists into strings using commas when necessary).
--
-- Used to serve the list of users to client.
type UserEntry = M.Map ByteString ByteString


------------------------------------------------------------------------------
-- | List of entries for all users present in the database, used to
-- serve user DB to client.
--
-- Previously known as @UsersDict@.
data UsersList = UsersList [UserEntry]
                 deriving (Show)

$(deriveToJSON id ''UsersList)


------------------------------------------------------------------------------
-- | Convert user meta to a map with all-string values, adding @value@
-- and @label@ keys for login and realName meta values, respectively.
toEntry :: Text
        -- ^ User login.
        -> UserMeta
        -> UserEntry
toEntry login meta =
    (M.insert "value" $ encodeUtf8 login) $
    (M.insert "label" $ encodeUtf8 $
      fromMaybe login $ realName meta) $
    (M.insert "roles" $ intercalate "," $
      map (\(Role r) -> r) $ metaRoles meta) $
    (M.insert "boCities" $ intercalate "," $
      fromMaybe [] $ boCities meta) $
    (M.insert "boPrograms" $ intercalate "," $
      fromMaybe [] $ boPrograms meta) $
    M.empty


------------------------------------------------------------------------------
-- | Convert user meta entry to a map with all-string values, as used
-- by legacy user meta from Snap authentication system.
toSnapMeta :: UserEntry -> HashMap Text Value
toSnapMeta = HM.fromList .
             map (\(k, v) -> (decodeUtf8 k, String $ decodeUtf8 v)) .
             M.toList


------------------------------------------------------------------------------
-- | Get list of all users from the database.
usersListPG :: HasPostgres m => m UsersList
usersListPG = do
  rows <- query_ allUsersQuery
  return $ UsersList $ map toEntry' rows
      where
        toEntry' :: ((Only Text) :. UserMeta) -> UserEntry
        toEntry' ((Only login) :. meta) = toEntry login meta


------------------------------------------------------------------------------
-- | Replace roles and meta for a user with those stored in Postgres.
replaceMetaRolesFromPG :: HasPostgres m => AuthUser -> m AuthUser
replaceMetaRolesFromPG user = do
  ur <- userRolesPG user
  umRes <- userMetaPG user
  let login = userLogin user
      um' = case umRes of
              Just um -> toSnapMeta $ toEntry login um
              Nothing -> userMeta user
  return user{userRoles = ur, userMeta = um'}
