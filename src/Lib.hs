{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( patternCount,
      increment,
      histogram,
      splitIntoKMers,
      histogramMax,
      kMersHistogram,
      kMerHistogram,
      mostFrequentKMers,
      frequentKMers,
      compliment,
      reverseCompliment,
      patternToNumber,
      numberToPattern,
      expand,
      frequencyArray,
      patternPositions,
      showArray,
      findClumps,
      skew,
      minSkewIndices,
      hammingDistance,
      approximatePatternPositions,
      approximatePatternCount,
      neighbors,
      frequentWordsWithMismatches
    ) where

import Data.String.Utils
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Foldable as F

import Data.List
import Data.List (intercalate)

import Data.Array
import Text.Regex.Base
import Text.Regex.PCRE


showArray :: Show a => [a] -> String
showArray = intercalate " " . map show

patternCount' :: [Char] -> [Char] -> Int -> Int
patternCount' pattern [] a = a
patternCount' pattern text a
  | startswith pattern text = patternCount' pattern (tail text) (a+1)
  | otherwise = patternCount' pattern (tail text) a

patternCount :: [Char] -> [Char] -> Int
patternCount pattern text = patternCount' pattern text 0

increment :: [Char] -> (M.Map [Char] Int) -> (M.Map [Char] Int)
increment k m = M.insertWith (+) k 1 m

histogram' :: [[Char]] -> (M.Map [Char] Int) -> (M.Map [Char] Int)
histogram' [] m = m
histogram' (x:xs) m = histogram' xs (increment x m)

histogram :: [[Char]] -> (M.Map [Char] Int)
histogram l = histogram' l M.empty

splitIntoKMers' :: [Char] -> Int -> [[Char]] -> [[Char]]
splitIntoKMers' [] k r = reverse r
splitIntoKMers' text k r
   | length text < k = reverse r
   | otherwise = splitIntoKMers' (drop 1 text) k ((take k text):r)

splitIntoKMers :: [Char] -> Int -> [[Char]]
splitIntoKMers text k = splitIntoKMers' text k []


kMerHistogram' :: [Char] -> Int -> (M.Map [Char] Int) -> Int -> Int -> (M.Map [Char] Int)
kMerHistogram' [] k acc pos stop = acc
kMerHistogram' text k acc pos stop
   | pos >= stop = acc
   | otherwise = kMerHistogram' (drop 1 text) k (increment (take k text) acc) (pos+1) stop

kMerHistogram :: [Char] -> Int -> (M.Map [Char] Int)
kMerHistogram text k = kMerHistogram' text k M.empty 0 ((length text) - k)

----------------------
test' :: [Char] -> Int -> Int -> Int
test' [] k i = i
test' text k i
   | length text < k = i
   | otherwise = test' (drop 1 text) k (i+1)

test :: [Char] -> Int -> Int
test text k = test' text k 0

length' :: [Char] -> Int -> Int
length' [] i = i
length' text i
  | length text < 1 = i
  | otherwise = length' (drop 1 text) (i+1)



--------------------------

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

frequentKMers ::  [Char] -> Int -> Int -> [[Char]]
frequentKMers text k m = M.keys $ M.filter (\v -> v >= m ) (kMersHistogram text k)

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

expand' ::  Int -> Int -> (M.Map [Char] Int) -> (M.Map [Char] Int)
expand' n k m
  | n == 4^k-1 = M.insert (numberToPattern n k) 0 m
  | otherwise = M.insert (numberToPattern n k) 0 (expand' (n+1) k m)

expand :: Int -> (M.Map [Char] Int)
expand k = expand' 0 k M.empty

frequencyArray :: [Char] -> Int -> [Int]
frequencyArray text k = M.elems $ histogram' (splitIntoKMers text k) (expand k)

patternPositions' :: [Char] -> [Char] -> [Int] -> Int -> [Int]
patternPositions' text pattern acc pos
  | (length text) < (length pattern) = reverse acc
  | take (length pattern) text == pattern = patternPositions' (drop 1 text) pattern (pos:acc) (pos+1)
  | otherwise = patternPositions' (drop 1 text) pattern acc (pos+1)

patternPositions :: [Char] -> [Char] -> [Int]
patternPositions text pattern = map (\i -> fst i) (getAllMatches $ (text =~ ("(?=("++pattern++")).") :: AllMatches [] (MatchOffset, MatchLength)))

findClumps :: [Char] -> Int -> Int -> Int -> [[Char]]
findClumps text k l t = frequentKMers text k t

skew' :: [Char] -> [Int] -> [Int]
skew' [] acc = reverse acc
skew' (x:xs) acc
  | x == 'G' = skew' xs ((head acc + 1):acc)
  | x == 'C' = skew' xs ((head acc - 1):acc)
  | otherwise = skew' xs ((head acc ):acc)

skew :: [Char] -> [Int]
skew t = skew' t [0]

minSkewIndices'' :: [Int] -> Int -> [Int]
minSkewIndices'' s m = findIndices (\i -> i == m) s

minSkewIndices' :: [Int] -> [Int]
minSkewIndices' s = minSkewIndices'' s (minimum s)

minSkewIndices :: [Char] -> [Int]
minSkewIndices t = minSkewIndices' (skew t)

hammingDistance' :: [Char] -> [Char] -> Int -> Int
hammingDistance' [] (y:ys) acc = hammingDistance' [] ys (acc + 1)
hammingDistance' (x:xs) [] acc = hammingDistance' xs [] (acc + 1)
hammingDistance' [] [] acc = acc
hammingDistance' (x:xs) (y:ys) acc
  | x /= y = hammingDistance' xs ys (acc + 1)
  | otherwise = hammingDistance' xs ys acc

hammingDistance :: [Char] -> [Char] -> Int
hammingDistance a b = hammingDistance' a b 0

-- TODO : Make this more efficent by not calcing the hamming distance
-- each time but doing a running hamming count.Can this be done??
approximatePatternPositions' :: [Char] -> [Char] -> Int -> [Int] -> Int -> [Int]
approximatePatternPositions' _ [] _ acc _ = reverse acc
approximatePatternPositions' p (x:xs) d acc pos
  | hammingDistance (take (length p) (x:xs)) p <= d = approximatePatternPositions' p xs d (pos:acc) (pos+1)
  | otherwise = approximatePatternPositions' p xs d acc (pos+1)

approximatePatternPositions :: [Char] -> [Char] -> Int -> [Int]
approximatePatternPositions p t d = approximatePatternPositions' p t d [] 0

approximatePatternCount :: [Char] -> [Char] -> Int -> Int
approximatePatternCount p t d = length $ approximatePatternPositions p t d


--------------------
-- TODO: Define a dictionary of characters ATCG
patternToNumber' :: [Char] -> Int -> Int -> Int
patternToNumber' [] _ _ = 0
patternToNumber' (x:[]) k c
  | x == 'A' = 4^(k-c) * 0
  | x == 'T' = 4^(k-c) * 1
  | x == 'G' = 4^(k-c) * 2
  | x == 'C' = 4^(k-c) * 3
patternToNumber' (x:xs) k c = (patternToNumber' [x] k c) + (patternToNumber' xs k (c-1))

patternToNumber :: [Char] -> Int
patternToNumber p = round $ (patternToNumber' p (length p) ((length p)-1))/4

numberToPattern'' :: Integer -> Char
numberToPattern'' n
  | n == 0 = 'A'
  | n == 1 = 'T'
  | n == 2 = 'G'
  | n == 3 = 'C'
  | otherwise = (show n) !! 0

numberToPattern' :: Integer -> Int -> Int -> [Char]
numberToPattern' n k i
  | k==i = []
  | otherwise =
      [numberToPattern'' $ toInteger $ floor (fromIntegral n/ fromIntegral 4^(k-i))]
      ++ numberToPattern' (fromIntegral n `mod` fromIntegral 4^(k-i)) k (i+1)

numberToPattern :: Int -> Int -> [Char]
numberToPattern n k = numberToPattern' (fromIntegral (n*4)) k 0



nucleotidePrepend :: [Char] -> [Char] -> [Char] -> Int  -> [[Char]]
nucleotidePrepend op p t d
  | (hammingDistance p t) < d = map (\i -> i:t) ['A','C','G','T']  --
  | otherwise = [(head op) : t]

neighbors :: [Char] -> Int -> [[Char]]
neighbors p 0 = [p]
neighbors p d
  | length p == 1 = ["A","C","G","T"]
  | otherwise = concat $ map (\i -> nucleotidePrepend p (tail p) i d ) (neighbors (tail p) d)

initLookup :: Int -> S.Seq Int
initLookup k = S.fromList $ map (\i->0) [0..(floor (4**fromIntegral(k))) - 1]

maxInLookup :: S.Seq Int -> Int
maxInLookup l = foldl (\a b -> if b>a then b else a) 0 l

updateCloseWithNeighbors :: S.Seq Int -> [Char] -> Int -> S.Seq Int
updateCloseWithNeighbors close text d =
  foldl
    (\a i -> S.update (patternToNumber i) (S.index a (patternToNumber i)) a)
    close
    (neighbors text d)

extractFrequentWords :: Int -> [Int] -> Int -> [[Char]] -> [[Char]]
extractFrequentWords _ [] _ acc = reverse acc
extractFrequentWords cutoff (l:ls) k acc
  | l >= cutoff = extractFrequentWords cutoff ls k ((numberToPattern l k):acc)
  | otherwise = extractFrequentWords cutoff ls k acc

frequentWordsWithMismatches' :: [Char] -> Int -> Int -> Int -> S.Seq Int -> [[Char]]
frequentWordsWithMismatches' text k d cnt close
  | cnt == 0 = extractFrequentWords (maxInLookup close) (F.toList close) k []
  | otherwise = frequentWordsWithMismatches'
      (drop 1 text) k d
      (cnt - 1)
      (updateCloseWithNeighbors close (take k text) d)

frequentWordsWithMismatches :: [Char] -> Int -> Int ->[[Char]]
frequentWordsWithMismatches text k d =
  frequentWordsWithMismatches'
    text k d
    ((length text) - k) -- End position
    (initLookup k)      -- Close
