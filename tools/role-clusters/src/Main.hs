{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

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
class (Ord e, Ord (Key e)) => MaskContainer e where
    type Key e
    getMasks :: e -> Map.Map (Key e) Mask
    setMasks :: Map.Map (Key e) Mask -> e -> e


-- | Add two access masks.
--
-- Remember to rescale afterwards.
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


-- | A set of (possibly quantum) field permissions for a role
data RolePerms = RolePerms { role  :: String
                           , rPerms :: Map.Map (String, String) Mask
                           }
                 deriving (Eq, Ord)


instance MaskContainer RolePerms where
    type Key RolePerms = (String, String)
    getMasks     = rPerms
    setMasks e f = f{rPerms = e}


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
toRolePerms :: [Permission] -> [PE RolePerms]
toRolePerms =
    map (\(k, v) -> PE $ RolePerms k v) .
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


-- | A wrapper for clusterizable mask containers.
newtype PE e = PE e deriving (Eq, Ord)


instance Show e => Show (PE e) where
    show (PE e) = show e


instance MaskContainer e => Element (PE e) where
    type Distance (PE e) = Double
    distance (PE f1) (PE f2) =
        sqrt $
        List.foldl' (\s (Q e1, Q e2) -> s + e1 ** 2 + e2 ** 2) 0
        (Map.elems $ getMasks (add f1 (scale (-1) f2)))
    average s =
        let
            l = map (\(PE e) -> e) $ Set.toList s
            n = Set.size s
            -- Add all available roles to FieldPerms
            totaled = map (totalize $ fullKeySet l) l
        in
          PE $ if (length l == 1)
               then head l
               else scale (1 / fromIntegral n) $ List.foldl1' add l


instance Show FieldPerms where
    show (FieldPerms m f perms) =
        concat [m, "/", f, " [", show $ Map.size perms, "]"]


-- | Sort all raw permissions by roles.
toFieldPerms :: [Permission] -> [PE FieldPerms]
toFieldPerms =
    map (\((m, f), v) -> PE $ FieldPerms m f v) .
    toPermMap (\(Permission (_, model, field, _, _)) -> (model, field))
              (\(Permission (role, _, _, cr, cw)) ->
               (role, (fromRaw cr, fromRaw cw)))


main = do
  a <- getArgs
  let (pgInfo, mode, algo, threshold) =
          case a of
            (host:port:user:pw:db:mode:algo:threshold:_) ->
                (ConnectInfo host (read port) user pw db,
                 mode,
                 algo,
                 threshold)
            _ -> error "Usage: role-clusters HOST PORT USER PW DB ('role'|'field') ('min'|'km') THRESHOLD|NUM"
  conn <- connect pgInfo
  perms <- query_ conn
           [sql|SELECT role, model, field, r, w FROM "FieldPermission";|]
  case (algo, mode) of
    ("min", "role")  -> print $ clusterize (toRolePerms perms) $ read threshold
    ("min", "field") -> print $ clusterize (toFieldPerms perms) $ read threshold
    ("km", "role")   -> print =<< clusterizeMeans
                        (toRolePerms perms) (read threshold) 0.01 100
    ("km", "field")  -> print =<< clusterizeMeans 
                        (toFieldPerms perms) (read threshold) 0.01 100
    (_, _)           -> print "Unknown mode"
  return ()
