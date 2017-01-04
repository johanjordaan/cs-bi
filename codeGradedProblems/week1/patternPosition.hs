patternMatch' :: [Char] -> [Char] -> Int -> Int -> [Int] -> [Int]
patternMatch' pattern text pos endPos acc
  | pos > endPos = reverse acc
  | (take (length pattern) text) == pattern =
      patternMatch'
        pattern
        (drop 1 text)
        (pos+1)
        endPos
        (pos:acc)
  | otherwise =
      patternMatch'
        pattern
        (drop 1 text)
        (pos+1)
        endPos
        acc

patternMatch :: [Char] -> [Char] -> [Int]
patternMatch pattern text = patternMatch' pattern text 0 ((length text) - (length pattern)) []

main :: IO ()
main = do
    pattern <- getLine
    text <- getLine
    let printMe xs = foldr (++) "" (map (\i -> (show i) ++ " ") xs)
    putStrLn (printMe (patternMatch pattern text))
