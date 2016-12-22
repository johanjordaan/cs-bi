{-# LANGUAGE OverloadedStrings #-}
module BiLib (
  withinHammingDistance
) where

import qualified Data.Map as M

import Data.List (intercalate)

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

kMerHistogram' :: [Char] -> Int -> (M.Map [Char] Int) -> Int -> Int -> (M.Map [Char] Int)
kMerHistogram' [] k acc pos stop = acc
kMerHistogram' text k acc pos stop
   | pos >= stop = acc
   | otherwise = kMerHistogram' (drop 1 text) k (M.insertWith (+) (take k text) 1 acc) (pos+1) stop

kMerHistogram :: [Char] -> Int -> (M.Map [Char] Int)
kMerHistogram text k = kMerHistogram' text k M.empty 0 ((length text) - k)

split' :: Int -> [Char] -> (M.Map [Char] Int) -> ((M.Map [Char] Int),(M.Map [Char] Int))
split' d key m = M.partitionWithKey (\k a -> withinHammingDistance d key k) m

collapse' :: (M.Map [Char] Int) -> [Char] -> Int -> (M.Map [Char] Int) -> (M.Map [Char] Int)
collapse' h (key:[]) d acc = acc
collapse' h (key:keys) d acc = collapse' (snd (split' d [key] h)) keys d (M.insert (M.elemAt 0 (fst (split' d [key] h))))

collapse :: (M.Map [Char] Int) -> Int -> (M.Map [Char] Int)
collapse h d = collapse' h (M.keys h) d M.empty

kMerHistogramWithMismatches :: [Char] -> Int -> Int -> (M.Map [Char] Int)
kMerHistogramWithMismatches text k d = collapse (kMerHistogram text k) d
