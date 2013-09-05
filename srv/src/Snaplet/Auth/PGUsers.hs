{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{-|

Postgres-based roles & user meta storage for Snap authentication
system.

Roles are stored in @usermetatbl@ table as created from the @usermeta@
model.

Populating user roles and meta from PG:

> Just u <- with auth currentUser
> u' <- with db $ replaceMetaRolesFromPG u

User meta instances are currently read using DbLayer, which employ
Redis backend. This is undesirable since we have to maintain separate
SQL-based code for group select operations required for 'usersListPG'
helper. We will get rid of this limitation once all models are ported
to use Haskell-side descriptions (which may then be used to generate
corresponding 'FromRow' instances).

-}

module Snaplet.Auth.PGUsers
    ( -- * User roles & meta
      userRolesPG
    , UserMeta
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
import Data.ByteString.Char8 (ByteString, intercalate, pack)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Maybe
import Data.Map as M hiding (map)
import Data.HashMap.Strict as HM (HashMap, fromList)

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.SqlQQ

import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.PostgresqlSimple

import qualified Data.Vector as V

import Snaplet.DbLayer as DB
import Snaplet.DbLayer.Types


-- | A usermeta instance converted to a HashMap used by legacy user
-- meta of Snap authentication system.
--
-- HashMap values are guaranteed to use 'String' constructor of
-- 'Value'.
--
-- The following fields of usermeta are not present: @login@, @uid@.
--
-- New fields added: @mid@ for usermeta id, @value@ for login, @label@
-- for realName.
newtype UserMeta = UserMeta (HashMap Text Value) deriving (Show, ToJSON)


instance FromField [Role] where
    fromField f dat = (map Role . V.toList) <$> fromField f dat


instance FromField [ByteString] where
    fromField f dat = V.toList <$> fromField f dat


------------------------------------------------------------------------------
-- | Select meta for a user with uid given as a query parameter.
userRolesQuery :: Query
userRolesQuery = [sql|
SELECT roles FROM usermetatbl WHERE uid=?;
|]


------------------------------------------------------------------------------
-- | Select meta id for a user with uid given as a query parameter.
userMidQuery :: Query
userMidQuery = [sql|
SELECT id FROM usermetatbl WHERE uid=?;
|]


------------------------------------------------------------------------------
-- | Convert a usermeta instance as read from DbLayer to use with
-- Snap. Values always use 'String' constructor.
toSnapMeta :: Map ByteString ByteString -> HashMap Text Value
toSnapMeta usermeta =
    HM.fromList $
    map (\(k, v) -> (decodeUtf8 k, String $ decodeUtf8 v)) $
    M.toList $
    -- Strip internal fields
    M.delete "login" $
    M.delete "uid" $
    M.delete "id" $
    -- Add user meta id under mid key
    M.insert "mid" mid $
    -- Add dictionary-like fields (map login to realName)
    M.insert "value" login $
    M.insert "label" (fromMaybe login $ M.lookup "realName" usermeta) $
    usermeta
    where
      mid =
          fromMaybe (error $ "No id field in usermeta " ++ show usermeta) $
          M.lookup "id" usermeta
      login =
          fromMaybe (error $ "No login field in usermeta " ++ show usermeta) $
          M.lookup "login" usermeta


------------------------------------------------------------------------------
-- | Get list of roles from the database for a user.
userRolesPG :: HasPostgres m => AuthUser -> m [Role]
userRolesPG user =
    case userId user of
      Nothing -> return []
      Just (UserId uid) -> do
        rows <- query userRolesQuery (Only uid)
        return $ case rows of
          ((e:_):_) -> e
          _     -> []


------------------------------------------------------------------------------
-- | Get meta from the database for a user.
userMetaPG :: AuthUser -> DbHandler b (Maybe UserMeta)
userMetaPG user =
    case userId user of
      Nothing -> return Nothing
      Just (UserId uid) -> do
        mid' <- query userMidQuery (Only uid)
        case mid' of
          (((mid :: Int):_):_) -> do
            -- This will read usermeta instance from Redis. If we
            -- could only read Postgres rows to commits.
            res <- DB.read "usermeta" $ pack $ show mid
            return $ Just $ UserMeta $ toSnapMeta res
          _     -> return Nothing


------------------------------------------------------------------------------
-- | Replace roles and meta for a user with those stored in Postgres.
replaceMetaRolesFromPG :: AuthUser -> DbHandler b AuthUser
replaceMetaRolesFromPG user = do
  ur <- userRolesPG user
  umRes <- userMetaPG user
  let um' = case umRes of
              Just (UserMeta um) -> um
              Nothing -> userMeta user
  return user{userRoles = ur, userMeta = um'}


------------------------------------------------------------------------------
-- | List of entries for all users present in the database, used to
-- serve user DB to client.
--
-- Previously known as @UsersDict@.
data UsersList = UsersList [HM.HashMap ByteString ByteString]
                 deriving (Show)

$(deriveToJSON defaultOptions ''UsersList)


------------------------------------------------------------------------------
-- | Select logins and metas for all users.
allUsersQuery :: Query
allUsersQuery = [sql|
SELECT m.id, u.login, m.realName, m.roles, m.boCities, m.boPrograms
FROM usermetatbl m, snap_auth_user u
WHERE u.uid=m.uid;
|]


------------------------------------------------------------------------------
-- | Fetch list of all users from the database, return @(mid, value,
-- label)@ for every user as well as some extra meta values required
-- by client.
usersListPG :: HasPostgres m => m UsersList
usersListPG = do
  rows <- query_ allUsersQuery
  return $ UsersList $ map toEntry rows
      where
        toEntry :: (Int,
                    ByteString,
                    Maybe ByteString,
                    Maybe [Role],
                    Maybe [ByteString],
                    Maybe [ByteString])
                 -> HM.HashMap ByteString ByteString
        toEntry (mid, login, rn, rls, boC, boP) =
            HM.fromList
                  [ ("mid", pack $ show $ mid)
                  , ("value", login)
                  , ("label", fromMaybe login rn)
                  , ("roles",
                     intercalate "," (map (\(Role r) -> r) $ fromMaybe [] rls))
                  , ("boCities",
                     intercalate "," $ fromMaybe [] boC)
                  , ("boPrograms",
                     intercalate "," $ fromMaybe [] boP)
                  ]
