import qualified Data.List (sortBy,maximumBy,foldl,sort,filter)
import qualified Data.Ord (comparing)

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
main :: IO ()
main = do
    text <- getLine
    putStrLn $ reverseCompliment text
