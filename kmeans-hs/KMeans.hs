{--
 - kmeans-hs/KMeans.hs
 -
 - Implementation of the K means clustering algorithm.
 -
 - Will Badart <wbadart@github>
 - created: APR 2018
 -}

module KMeans
( minkowski
, kmeans
) where

import Data.Function (on)
import Data.List (minimumBy)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Point  = [Double]
type Metric = (Point -> Point -> Double)

minkowski :: Double -> Point -> Point -> Double
minkowski p pointA pointB =
   sum (zipWith absDistP pointA pointB) ** (1 / p)
  where
   absDistP xa xb = abs (xa - xb) ** p

closestCentroid :: Point -> Metric -> [Point] -> Point
closestCentroid point metric = minimumBy (compare `on` metric point)

kmeans :: [Point] -> Metric -> [Point] -> Map Point [Point]
kmeans centroids metric data_ =
    foldr assignCluster (M.fromList $ zip centroids (repeat [])) centroids
  where
    assignCluster point clustering =
      let target_centroid = closestCentroid point metric $ M.keys clustering
      in  M.adjust (point:) target_centroid clustering
