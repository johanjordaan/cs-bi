import qualified Data.List (sortBy,maximumBy,foldl,sort,filter)
import qualified Data.Ord (comparing)


patternToNumber' :: [Char] -> Int -> Int -> Int
patternToNumber' [] _ _ = 0
patternToNumber' (x:[]) k c
  | x == 'A' = 4^(k-c) * 0
  | x == 'C' = 4^(k-c) * 1
  | x == 'G' = 4^(k-c) * 2
  | x == 'T' = 4^(k-c) * 3
patternToNumber' (x:xs) k c = (patternToNumber' [x] k c) + (patternToNumber' xs k (c-1))

patternToNumber :: [Char] -> Int
patternToNumber p = round $ (fromIntegral (patternToNumber' (reverse p) (length p) ((length p)-1))) / 4

numberToPattern'' :: Integer -> Char
numberToPattern'' n
  | n == 0 = 'A'
  | n == 1 = 'C'
  | n == 2 = 'G'
  | n == 3 = 'T'
  | otherwise = (show n) !! 0

numberToPattern' :: Integer -> Int -> Int -> [Char]
numberToPattern' n k i
  | k==i = []
  | otherwise =
      (numberToPattern'' $ toInteger $ floor (fromIntegral n/ fromIntegral 4^(k-i)))
      : (numberToPattern' (fromIntegral n `mod` fromIntegral 4^(k-i)) k (i+1))

numberToPattern :: Int -> Int -> [Char]
numberToPattern n k = numberToPattern' (fromIntegral (n*4)) k 0

countOccurencesInSortedList' :: [Int] -> Int -> Int -> Int -> [([Char],Int)] -> [([Char],Int)]
countOccurencesInSortedList' [] _ _ _ acc = reverse $ Data.List.sortBy (Data.Ord.comparing snd) acc
countOccurencesInSortedList' (x:[]) k _ count acc = (numberToPattern x k,count+1):acc
countOccurencesInSortedList' (x:xs) k prevX count acc
  | prevX /= x = countOccurencesInSortedList' xs k x 1 ((numberToPattern prevX k,count):acc)
  | otherwise = countOccurencesInSortedList' xs k x (count+1) acc

countOccurencesInSortedList :: [Int] -> Int -> [([Char],Int)]
countOccurencesInSortedList acc k =
  countOccurencesInSortedList'
    (Data.List.sort acc)
    k
    ((Data.List.sort acc)!!0)   -- prevX
    1   -- count
    []  -- acc


--Data.List.maximumBy (Data.Ord.comparing snd) x
frequentWordsBySorting' :: [Char] -> Int -> Int -> Int -> [Int] -> [[Char]]
frequentWordsBySorting' text k pos endPos acc
  | pos > endPos =
      Data.List.foldl
        (\a i -> (fst i):a)
        []
        (
          Data.List.filter
            (\i -> (snd i) ==
              snd (
                Data.List.maximumBy (Data.Ord.comparing snd) (countOccurencesInSortedList acc k)
              )
            )
            (countOccurencesInSortedList acc k)
        )
  | otherwise = frequentWordsBySorting' (drop 1 text) k (pos+1) endPos ((patternToNumber (take k text)):acc)

frequentWordsBySorting :: [Char] -> Int -> [[Char]]
frequentWordsBySorting text k = frequentWordsBySorting' text k 0 ((length text)-k) []

main :: IO ()
main = do
    text <- getLine
    k <- getLine
    let printMe xs = foldr (++) "" (map (\str -> str ++ " ") xs)
    putStrLn (printMe (frequentWordsBySorting text (read k :: Int)))
