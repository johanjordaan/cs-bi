{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( patternCount,
      increment,
      histogram,
      splitIntoKMers,
      mostFrequentKMers
    ) where

import Data.String.Utils
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as M

patternCount' :: [Char] -> [Char] -> Int -> Int
patternCount' pattern [] a = a
patternCount' pattern text a
  | startswith pattern text = patternCount' pattern (tail text) (a+1)
  | otherwise = patternCount' pattern (tail text) a

patternCount :: [Char] -> [Char] -> Int
patternCount pattern text = patternCount' pattern text 0

increment :: [Char] -> (M.Map [Char] Int) -> (M.Map [Char] Int)
increment k m = M.insert k ((M.findWithDefault 0 k m ) +1) m

histogram' :: [[Char]] -> (M.Map [Char] Int) -> (M.Map [Char] Int)
histogram' [] m = m
histogram' (x:xs) m = histogram' xs (increment x m)

histogram :: [[Char]] -> (M.Map [Char] Int)
histogram l = histogram' l M.empty


drop' k text = C.unpack $ C.drop (fromIntegral k) $ C.pack text
take' k text = C.unpack $ C.take (fromIntegral k) $ C.pack text

splitIntoKMers' :: [Char] -> Int -> [[Char]] -> [[Char]]
splitIntoKMers' [] k r = r
splitIntoKMers' text k r
   | length text < k = r
   | otherwise = splitIntoKMers' (drop' 1 text) k (r ++ [(take' k text)])

splitIntoKMers :: [Char] -> Int -> [[Char]]
splitIntoKMers text k = splitIntoKMers' text k []

mostFrequentKMers :: Int -> [Char] -> [[Char]]
mostFrequentKMers text k = histogram $ splitIntoKMers text k


--["CATG","GCAT"]
