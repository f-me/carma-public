{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

{-|

Postgres-based roles & user meta storage for Snap authentication
system.

Roles are stored in @usermetatbl@ table as created from the @usermeta@
model.

Populating user roles and meta from PG:

> Just u <- with auth currentUser
> u' <- with db $ replaceMetaRolesFromPG u

Obtain Usermeta id from Snap user:

> Just u <- with auth currentUser
> Just (i, _) <- with db $ userMetaPG u

Usermeta ids are preferred over Snap user when a reference to a user
is stored.

User meta instances are currently read using DbLayer, which employs
Redis backend. This is undesirable since we have to maintain separate
SQL-based code for group select operations required for 'usersListPG'
helper. We will get rid of this limitation once all models are ported
to use Haskell-side descriptions (which may then be used to generate
corresponding 'FromRow' instances) (TODO #1462).

-}

module Snaplet.Auth.PGUsers
    ( -- * User roles & meta
      userRolesPG
    , UserMeta
    , userMetaPG
    , replaceMetaRolesFromPG
    )

where

import Control.Applicative

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
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


instance FromField [Text] where
    fromField f dat = V.toList <$> fromField f dat


------------------------------------------------------------------------------
-- | Select meta for a user with uid given as a query parameter.
userRolesQuery :: Query
userRolesQuery = [sql|
SELECT roles::text[] FROM usermetatbl WHERE uid=?;
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
toSnapMeta :: Map Text Text -> HashMap Text Value
toSnapMeta usermeta =
    HM.fromList $
    map (\(k, v) -> (k, String v)) $
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
userMetaPG :: AuthUser -> DbHandler b (Maybe (Int, UserMeta))
userMetaPG user =
    case userId user of
      Nothing -> return Nothing
      Just (UserId uid) -> do
        mid' <- query userMidQuery (Only uid)
        case mid' of
          ((mid:_):_) -> do
            -- This will read usermeta instance from Redis.
            --
            -- TODO If we could only read Postgres rows to commits.
            res <- DB.read "usermeta" $ T.pack $ show mid
            return $ Just (mid, UserMeta $ toSnapMeta res)
          _     -> return Nothing


------------------------------------------------------------------------------
-- | Replace roles and meta for a user with those stored in Postgres.
replaceMetaRolesFromPG :: AuthUser -> DbHandler b AuthUser
replaceMetaRolesFromPG user = do
  ur <- userRolesPG user
  umRes <- userMetaPG user
  let um' = case umRes of
              Just (_, UserMeta um) -> um
              Nothing -> userMeta user
  return user{userRoles = ur, userMeta = um'}
