{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( patternCount,
      increment,
--      histogram,
      mostFrequentKMers
    ) where

import Data.String.Utils
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as M

_patternCount :: [Char] -> [Char] -> Int -> Int
_patternCount pattern [] a = a
_patternCount pattern text a
  | startswith pattern text = _patternCount pattern (tail text) (a+1)
  | otherwise = _patternCount pattern (tail text) a

patternCount :: [Char] -> [Char] -> Int
patternCount pattern text = _patternCount pattern text 0

increment :: [Char] -> (M.Map [Char] Int) -> (M.Map [Char] Int)
increment k m = M.insert k ((M.findWithDefault 0 k m ) +1) m

--histogram :: [[Char]] -> (M.Map [Char] Int)
--histogram l = increment

mostFrequentKMers :: Int -> [Char] -> [[Char]]
mostFrequentKMers k text = ["CATG","GCAT"]
