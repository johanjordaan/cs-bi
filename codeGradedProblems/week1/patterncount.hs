patternCount' :: [Char] -> [Char] -> Int -> Int
patternCount' [] pattern count = count
patternCount' text pattern count
  | (take (length pattern) text) == pattern = patternCount' (drop 1 text) pattern (count+1)
  | otherwise = patternCount' (drop 1 text) pattern count

patternCount :: [Char] -> [Char] -> Int
patternCount text pattern = patternCount' text pattern 0

main :: IO ()
main = do
    text <- getLine
    pattern <- getLine
    putStrLn $ show $ patternCount text pattern
