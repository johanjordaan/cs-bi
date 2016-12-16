{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( patternCount,
      increment,
      histogram,
      splitIntoKMers,
      histogramMax,
      kMersHistogram,
      mostFrequentKMers,
      compliment,
      reverseCompliment,
      patternToNumber,
      numberToPattern,
      expand,
      frequencyArray--,
      --patternPositions
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

pickMax' :: Int -> Int -> Int
pickMax' a b
  | a > b = a
  | otherwise = b

histogramMax :: (M.Map [Char] Int) -> Int
histogramMax m = M.fold pickMax' 0 m

kMersHistogram :: [Char] -> Int -> (M.Map [Char] Int)
kMersHistogram text k = histogram $ splitIntoKMers text k

mostFrequentKMers ::  [Char] -> Int -> [[Char]]
mostFrequentKMers text k = M.keys $ M.filter (\v -> v == (histogramMax $ kMersHistogram text k)) (kMersHistogram text k)

complimentOne :: Char -> [Char]
complimentOne x
  | x == 'A' = "T"
  | x == 'T' = "A"
  | x == 'C' = "G"
  | x == 'G' = "C"

compliment :: [Char] -> [Char]
compliment [] = []
compliment (x:xs) =  complimentOne x ++ compliment xs

reverseCompliment :: [Char] ->[Char]
reverseCompliment s = reverse $ compliment s

patternToNumber' :: [Char] -> Int -> Int -> Int
patternToNumber' [] _ _ = 0
patternToNumber' (x:[]) k c
  | x == 'A' = 4^(k-c) * 0
  | x == 'T' = 4^(k-c) * 1
  | x == 'G' = 4^(k-c) * 2
  | x == 'C' = 4^(k-c) * 3
patternToNumber' (x:xs) k c = (patternToNumber' [x] k c) + (patternToNumber' xs k (c-1))

patternToNumber :: [Char] -> Int
patternToNumber p = patternToNumber' p (length p) ((length p)-1)

numberToPattern'' :: Integer -> Char
numberToPattern'' n
  | n == 0 = 'A'
  | n == 1 = 'C'
  | n == 2 = 'G'
  | n == 3 = 'T'

numberToPattern' :: Integer -> Int -> Int -> [Char]
numberToPattern' n k i
  | k==i = [numberToPattern'' n]
  | otherwise =
      [numberToPattern'' $ toInteger $ floor (fromIntegral n/ fromIntegral 4^(k-i))]
      ++ numberToPattern' (fromIntegral n `mod` fromIntegral 4^(k-i)) k (i+1)

numberToPattern :: Integer -> Int -> [Char]
numberToPattern n k = numberToPattern' n k 1

expand' ::  Integer -> Int -> (M.Map [Char] Int) -> (M.Map [Char] Int)
expand' n k m
  | n == toInteger 4^k-1 = M.insert (numberToPattern n k) 0 m
  | otherwise = M.insert (numberToPattern n k) 0 (expand' (n+1) k m)

expand :: Int -> (M.Map [Char] Int)
expand k = expand' 0 k M.empty

frequencyArray :: [Char] -> Int -> [Int]
frequencyArray text k = M.elems $ histogram' (splitIntoKMers text k) (expand k)

--patternPositions :: [Char] -> [Char] -> [Int]
