{-# LANGUAGE OverloadedStrings #-}
module BiLib
  (
    withinHammingDistance
  , hammingDistance
  ) where

import qualified Data.Map as M

import Data.List
  ( intercalate
  , intersect
  )

withinHammingDistance' :: Int -> [Char] -> [Char] -> Int -> Bool
withinHammingDistance' d [] (y:ys) acc
  | acc > d = False
  | otherwise = withinHammingDistance' d [] ys (acc + 1)
withinHammingDistance' d (x:xs) [] acc
  | acc > d = False
  | otherwise = withinHammingDistance' d xs [] (acc + 1)
withinHammingDistance' d [] [] acc = acc <= d
withinHammingDistance' d (x:xs) (y:ys) acc
  | acc > d = False
  | x /= y = withinHammingDistance' d xs ys (acc + 1)
  | otherwise = withinHammingDistance' d xs ys acc

withinHammingDistance :: Int -> [Char] -> [Char] -> Bool
withinHammingDistance d a b = withinHammingDistance' d a b 0

hammingDistance' :: [Char] -> [Char] -> Int -> Int
hammingDistance' [] (y:ys) acc = hammingDistance' [] ys (acc + 1)
hammingDistance' (x:xs) [] acc = hammingDistance' xs [] (acc + 1)
hammingDistance' [] [] acc = acc
hammingDistance' (x:xs) (y:ys) acc
  | x /= y = hammingDistance' xs ys (acc + 1)
  | otherwise = hammingDistance' xs ys acc

hammingDistance :: [Char] -> [Char] -> Int
hammingDistance a b = hammingDistance' a b 0

kMerHistogram' :: [Char] -> Int -> (M.Map [Char] Int) -> Int -> Int -> (M.Map [Char] Int)
kMerHistogram' [] k acc pos stop = acc
kMerHistogram' text k acc pos stop
   | pos > stop = acc
   | otherwise = kMerHistogram' (drop 1 text) k (M.insertWith (+) (take k text) 1 acc) (pos+1) stop

kMerHistogram :: [Char] -> Int -> (M.Map [Char] Int)
kMerHistogram text k = kMerHistogram' text k M.empty 0 ((length text) - k)


-- Add all the counts calculate the consenus key
summarise' :: (M.Map [Char] Int) -> ([Char],Int)
summarise' m
  | M.null m = ("XXX",0)
  | otherwise =
    (
      fst $ M.elemAt 0 m
    , M.foldl (+) 0 m
    )

nucleotidePrepend :: [Char] -> [Char] -> [Char] -> Int  -> [[Char]]
nucleotidePrepend op p t d
  | (hammingDistance p t) < d = map (\i -> i:t) ['A','C','G','T']  --
  | otherwise = [(head op) : t]

neighbors :: [Char] -> Int -> [[Char]]
neighbors p 0 = [p]
neighbors p d
  | length p == 1 = ["A","C","G","T"]
  | otherwise = concat $ map (\i -> nucleotidePrepend p (tail p) i d ) (neighbors (tail p) d)

-- Strings are defined as neighbors if they shares at least one neighbor
-- TODO: Could be more efficient since I don't need all the neighbors or all
-- the intersects. So neighbors could be changed to yield results and intersect
-- can be made to terminate early
areNeighbors :: [Char] -> [Char] -> Int -> Bool
areNeighbors a b d = length (intersect (neighbors a d) (neighbors b d)) > 0

split' :: Int -> [Char] -> (M.Map [Char] Int) -> ((M.Map [Char] Int),(M.Map [Char] Int))
split' d key m = M.partitionWithKey (\k a -> areNeighbors key k d) m

collapse' :: (M.Map [Char] Int) -> [[Char]] -> Int -> (M.Map [Char] Int) -> (M.Map [Char] Int)
collapse' _ [] _ acc = acc
collapse' h (key:[]) d acc
  | M.null h = acc
  | otherwise = uncurry M.insert (summarise' h) acc
collapse' h (key:keys) d acc
  | M.null h = acc
  | otherwise = collapse'
      (snd (split' d key h))
      keys
      d
      (uncurry M.insert (summarise' (fst (split' d key h))) acc)

collapse :: (M.Map [Char] Int) -> Int -> (M.Map [Char] Int)
collapse h d = collapse' h (M.keys h) d M.empty

kMerHistogramWithMismatches :: [Char] -> Int -> Int -> (M.Map [Char] Int)
kMerHistogramWithMismatches text k d = collapse (kMerHistogram text k) d
