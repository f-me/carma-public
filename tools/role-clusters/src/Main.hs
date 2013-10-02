{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import System.Environment

import Cluster


-- | A single line in FieldPermission table (role, model, field, read, write)
newtype Permission = Permission (String, String, String, Bool, Bool)
                     deriving (FromRow, Show)


-- | Quantum access mode
data Access = Yes | No | Q Double deriving (Eq, Ord)


fromRaw :: Bool -> Access
fromRaw True = Yes
fromRaw False = No


symDifference :: Ord e => Set.Set e -> Set.Set e -> Set.Set e
symDifference s1 s2 = Set.difference (Set.union s1 s2) (Set.intersection s1 s2)


-- | A set of (possibly quantum) permissions for a role
data RolePerms = RolePerms { role  :: String
                           , rPerms :: Set.Set (String, String, Access, Access)
                           }
                 deriving Eq


instance Element RolePerms where
    type Distance RolePerms = Int
    distance p1 p2 = Set.size $ symDifference (rPerms p1) (rPerms p2)


instance Show RolePerms where
    show (RolePerms r perms) =
        concat [r, " [", show $ Set.size perms, "]"]


-- | Sort all raw permissions by roles.
toRolePerms :: [Permission] -> [RolePerms]
toRolePerms perms =
    map (\(k, v) -> RolePerms k v) $
    Map.toList $
    List.foldl' addPerm Map.empty perms
    where
      addPerm set (Permission (role, model, field, canRead, canWrite)) =
          Map.insertWith Set.union role
          (Set.singleton (model, field, fromRaw canRead, fromRaw canWrite))
          set


-- | A set of (possibly quantum) roles for a field
data FieldPerms = FieldPerms { model :: String
                             , field :: String
                             , fPerms :: Set.Set (String, Access, Access)
                             }
                 deriving Eq


instance Element FieldPerms where
    type Distance FieldPerms = Int
    distance p1 p2 = Set.size $ symDifference (fPerms p1) (fPerms p2)


instance Show FieldPerms where
    show (FieldPerms m f perms) =
        concat [m, "/", f, " [", show $ Set.size perms, "]"]

-- | Sort all raw permissions by fields.
toFieldPerms :: [Permission] -> [FieldPerms]
toFieldPerms perms =
    map (\((m, f), v) -> FieldPerms m f v) $
    Map.toList $
    List.foldl' addPerm Map.empty perms
    where
      addPerm set (Permission (role, model, field, canRead, canWrite)) =
          Map.insertWith Set.union (model, field)
          (Set.singleton (role, fromRaw canRead, fromRaw canWrite))
          set


main = do
  a <- getArgs
  let pgInfo =
          case a of
            (host:port:user:pw:db:_) ->
                ConnectInfo host (read port) user pw db
            _ -> error "Usage: role-clusters HOST PORT USER PW DB"
  conn <- connect pgInfo
  perms <- query_ conn
           [sql|SELECT role, model, field, r, w FROM "FieldPermission";|]
  print (clusterize (toFieldPerms perms) 3)
  return ()
