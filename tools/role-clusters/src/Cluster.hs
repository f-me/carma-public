{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Cluster

where

import Control.Monad

import Data.List
import Data.Maybe

import qualified Data.Set as Set

import System.Random


type Elements e = Set.Set e


data Cluster e = Cluster { center :: e
                         , elements :: Elements e
                         }

instance Show e => Show (Cluster e) where
    show c = show (elements c) ++ " @ " ++ show (center c)

instance (Eq e, Ord e) => Eq (Cluster e) where
    (Cluster c1 e1) == (Cluster c2 e2) =
        e1 == e2 && c1 == c2


class (Eq e, Ord e,
       Fractional (Distance e),
       Ord (Distance e)) => Element e where
    type Distance e
    distance :: e -> e -> Distance e
    average :: Elements e -> e


addToCluster :: Ord e => Cluster e -> e -> Cluster e
addToCluster cluster elt = cluster{elements = Set.insert elt $ elements cluster}


-- | Make a new cluster with the same elements but using median point
-- of elements as the new center.
recenter :: Element e => Cluster e -> Cluster e
recenter cluster = cluster{center = average $ elements cluster}


newCluster :: Element e => e -> Cluster e
newCluster e = Cluster e Set.empty


-- | Classify a point according to minimum distance rule provided a
-- (possibly null) set of clusters.
classify :: Element e =>
            e
         -> [Cluster e]
         -- ^ Existing list of clusters. If no clusters exists, a new
         -- one will be created.
         -> Maybe (Distance e)
         -- ^ When given, a new cluster will be created if the minimum
         -- distance to any of existing clusters exceeds this value.
         -> [Cluster e]
classify point clusters threshold =
    let
        distances = zip [distance point (center c) | c <- clusters] [0..]
        -- minDist is a pair of minimal distance to cluster and its index;
        -- if no cluster is within threshold radius is found, index is -1
        minDist = foldl (\p n -> if (fst p) <= (fst n) then p else n)
                  (t, -1) distances
        t = fromMaybe (1.0 / 0.0) threshold
        clusterId = snd minDist
    in
      if clusterId == -1 then
      -- Add new cluster around this point
          (Cluster point $ Set.singleton point):clusters
      else
      -- Add point to existing cluster
        replaceIdx clusters clusterId
                       (addToCluster (clusters !! clusterId) point)


-- | Single-pass minimum distance clustering (possibly using a
-- predefined list of known clusters)
clusterize1 :: Element e =>
               [e]
            -> [Cluster e]
            -> Maybe (Distance e)
            -> [Cluster e]
clusterize1 (p:points) clusters threshold =
    clusterize1 points (classify p clusters threshold) threshold
clusterize1 [] clusters _ = clusters


-- | Single-pass minimum center distance clustering
clusterize :: Element e => [e] -> Distance e -> [Cluster e]
clusterize points threshold = clusterize1 points [] (Just threshold)


clusterizeMeans :: Element e =>
                   [e]
                -> Int
                -- ^ Desired number of clusters.
                -> Distance e
                -- ^ Cluster median minimum shift to ensure next iteration.
                -> Int
                -- ^ Maximum number of tries.
                -> IO [Cluster e]
clusterizeMeans points num threshold maxTries =
    let
        clusterizeMean1 points clusters i =
            let
                newCenters = map center (map recenter clusters)
                oldCenters = map center clusters
                -- newClusters with only centers
                newClusters = map newCluster newCenters
            in
              -- If recentered clusters are the same as on previous
              -- or maximum step reached, give current clustering
              if ((minimum $
                   map (uncurry distance) (zip newCenters oldCenters)) <=
                  threshold) || (i == 0) then
                  clusters
              else
                  clusterizeMean1 points
                  (clusterize1 points newClusters Nothing) (i - 1)
    in do
      -- Forgy initialization
      initialIds <- replicateM num $
                    getStdRandom (randomR (0, length points - 1))
      if (length $ nub initialIds) == num
      then return $ clusterizeMean1 points
               (clusterize1 points
                (map (\i -> newCluster (points !! i)) initialIds) Nothing)
               maxTries
      else clusterizeMeans points num threshold maxTries

-- | @replaceIdx l i e@ changes @i@-th element in @l@ to @e@. @i@ is
-- 1-based.
replaceIdx (x:xs) 0 e = e:xs
replaceIdx [] i e = []
replaceIdx (x:xs) i e = x:(replaceIdx xs (i - 1) e)
