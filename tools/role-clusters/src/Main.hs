{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Maybe

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
newtype Access = Q Double deriving (Eq, Ord, Show)


type Mask = (Access, Access)


fromRaw :: Bool -> Access
fromRaw True = Q 1.0
fromRaw False = Q 0.0


scaleMask :: Double -> Mask -> Mask
scaleMask s (Q q1, Q q2) = (Q (q1 * s), Q (q2 * s))


addMask :: Mask -> Mask -> Mask
addMask (Q q1, Q q2) (Q q1', Q q2') = (Q (q1 + q1'), Q (q2 + q2'))


-- | Datatypes storing a map of keyed r/w access masks.
class (Ord (Key e)) => MaskContainer e where
    type Key e
    getMasks :: e -> Map.Map (Key e) Mask
    setMasks :: Map.Map (Key e) Mask -> e -> e


-- | A set of (possibly quantum) permissions for a role
data RolePerms = RolePerms { role  :: String
                           , rPerms :: Map.Map (String, String) Mask
                           }
                 deriving (Eq, Ord)


instance Element RolePerms where
    type Distance RolePerms = Int


instance Show RolePerms where
    show (RolePerms r perms) =
        concat [r, " [", show $ Map.size perms, "]"]


-- | Sort raw permissions into an associative list
toPermMap :: (Ord k, Ord s, Ord v) =>
             (Permission -> k)
          -> (Permission -> (s, v))
          -> [Permission]
          -> [(k, Map.Map s v)]
toPermMap keyFun valFun perms =
    Map.toList $
    List.foldl' addPerm Map.empty perms
    where
      addPerm set p =
          Map.insertWith Map.union (keyFun p)
                 ((\(s, v) -> Map.singleton s v) $ valFun p) set


-- | Sort all raw permissions by roles.
toRolePerms :: [Permission] -> [RolePerms]
toRolePerms =
    map (\(k, v) -> RolePerms k v) .
    toPermMap (\(Permission (role, _, _, _, _)) -> role)
              (\(Permission (_, model, field, cr, cw)) ->
               ((model, field), (fromRaw cr, fromRaw cw)))


-- | A set of (possibly quantum) roles for a field
data FieldPerms = FieldPerms { model :: String
                             , field :: String
                             , fPerms :: Map.Map String Mask
                             }
                 deriving (Eq, Ord)


instance MaskContainer FieldPerms where
    type Key FieldPerms = String
    getMasks     = fPerms
    setMasks e f = f{fPerms = e}


-- | Add access masks. Remember to rescale.
add :: MaskContainer e => e -> e -> e
add f1 f2 = setMasks (Map.unionWith addMask (getMasks f1) (getMasks f2)) f1


scale :: MaskContainer e => Double -> e -> e
scale n f = setMasks (Map.map (scaleMask n) $ getMasks f) f


-- | Calculate universal set for keys in a list of MaskContainers
fullKeySet :: MaskContainer e => [e] -> [Key e]
fullKeySet l = List.nub $ concat $ map (Map.keys . getMasks) l


-- | Add missing keys (wrt universal set) to masks stored in a
-- container, producing unpacked mask map.
totalize :: MaskContainer e => [Key e] -> e -> [(Key e, Mask)]
totalize fullSet f =
    map (\r ->
         (r, fromMaybe (Q 0, Q 0) (Map.lookup r fp)))
    fullSet
    where
      fp = getMasks f


instance Element FieldPerms where
    type Distance FieldPerms = Double
    distance f1 f2 =
        sqrt $
        List.foldl' (\s (Q e1, Q e2) -> s + e1 ** 2 + e2 ** 2) 0
        (Map.elems $ fPerms (add f1 (scale (-1) f2)))
    average s =
        let
            l = Set.toList s
            n = Set.size s
            -- Add all available roles to FieldPerms
            totaled = map (totalize $ fullKeySet l) l
        in
          scale (1 / fromIntegral n) $ List.foldl1' add l

instance Show FieldPerms where
    show (FieldPerms m f perms) =
        concat [m, "/", f, " [", show $ Map.size perms, "]"]


-- | Sort all raw permissions by roles.
toFieldPerms :: [Permission] -> [FieldPerms]
toFieldPerms =
    map (\((m, f), v) -> FieldPerms m f v) .
    toPermMap (\(Permission (_, model, field, _, _)) -> (model, field))
              (\(Permission (role, _, _, cr, cw)) ->
               (role, (fromRaw cr, fromRaw cw)))


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
  print (clusterize (toFieldPerms perms) 3.0)
  return ()
