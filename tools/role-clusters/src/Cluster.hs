{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Cluster

where

import qualified Data.Set as Set


type Elements e = [e]


data Cluster e = Cluster { center :: e
                         , elements :: Elements e
                         }

instance Show e => Show (Cluster e) where
    show c = show (elements c) ++ " @ " ++ show (center c)

instance (Eq e, Ord e) => Eq (Cluster e) where
    (Cluster c1 e1) == (Cluster c2 e2) = 
        (Set.fromList e1 == Set.fromList e2) && c1 == c2


class (Eq e, Ord (Distance e)) => Element e where
    type Distance e
    distance :: e -> e -> Distance e
    average :: Elements e -> e


addToCluster :: Cluster e -> e -> Cluster e
addToCluster cluster elt = cluster{elements = elt:(elements cluster)}


-- | Make a new cluster with the same elements and threshold but using
-- median point of element as the new center.
recenter :: Element e => Cluster e -> Cluster e
recenter cluster = cluster{center = average $ elements cluster}


-- | Classify a point according to minimum distance rule provided a
-- (possibly null) set of clusters.
classify :: Element e => 
            e 
         -> [Cluster e] 
         -> Distance e 
         -> [Cluster e]
classify point clusters threshold = 
    let
        distances = zip [distance point (center c) | c <- clusters] [0..]
        -- minDist is a pair of minimal distance to cluster and its index;
        -- if no cluster is within threshold radius is found, index is -1
        minDist   = foldl (\p n -> if (fst p) <= (fst n) then p else n) (threshold, -1) distances
        clusterId = snd minDist
    in
      if clusterId == -1 then
      -- Add new cluster around this point
          (Cluster point [point]):clusters
      else
      -- Add point to existing cluster
        replaceIdx clusters clusterId (addToCluster (clusters !! clusterId) point)


-- | Single-pass minimum distance clustering (possibly using a
-- predefined list of known clusters)
clusterize1 :: Element e => 
               [e] 
            -> [Cluster e] 
            -> Distance e 
            -> [Cluster e]
clusterize1 (p:points) clusters threshold = clusterize1 points (classify p clusters threshold) threshold
clusterize1 [] clusters threshold = clusters


-- | Single-pass minimum distance clustering
clusterize :: Element e => [e] -> Distance e -> [Cluster e]
clusterize points threshold = clusterize1 points [] threshold


-- | k-means clustering
clusterizeMean :: Element e => [e] -> Distance e -> Int -> [Cluster e]
clusterizeMean points threshold maxTries =
    let
        clusterizeMean1 points clusters i =
            let
                newCenters = map center (map recenter clusters)
                oldCenters = map center clusters
                -- newClusters with only centers
                newClusters = map (\c -> Cluster c []) newCenters
            in
                -- If recentered clusters are the same as on previous
                -- or maximum step reached, give current clustering
                if (newCenters == oldCenters) || (i == 0) then
                   clusters
                else
                   clusterizeMean1 points (clusterize1 points newClusters threshold) (i - 1)
    in
        clusterizeMean1 points (clusterize points threshold) maxTries


-- | @replaceIdx l i e@ changes @i@-th element in @l@ to @e@. @i@ is
-- 1-based.
replaceIdx (x:xs) 0 e = e:xs
replaceIdx [] i e = []
replaceIdx (x:xs) i e = x:(replaceIdx xs (i - 1) e)
