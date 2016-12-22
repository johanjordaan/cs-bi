module Main where

import Lib

main :: IO ()
main = do
  ec <- readFile "./genomes/E_coli.txt"
  let m = kMerHistogram ec 15
  print $ length m
  putStrLn "Done"
