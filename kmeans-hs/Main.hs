{--
 - kmeans-hs/Main.hs
 -
 - Sample driver program for KMeans module.
 -
 - Will Badart <wbadart@github>
 - created: APR 2018
 -}

module Main (main) where

import Data.List.Split (splitOn)
import KMeans (kmeans, minkowski)

main = do
    buf <- readFile "cfb.tsv"
    let df       = map parse . tail . map (splitOn "\t") . lines $ buf
        clusters = kmeans [[7, 7], [14, 14]] (minkowski 2) df
    print clusters
  where
    parse :: [String] -> [Double]
    parse = take 2 . map read . tail
