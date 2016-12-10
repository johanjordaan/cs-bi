import Test.Hspec
import Control.Exception (evaluate)
import qualified Data.Map as M
import Lib (patternCount, increment, histogram, splitIntoKMers, mostFrequentKMers)


main :: IO ()
main = hspec $ do
   describe "patterCount" $ do
      it "returns the number of occurences of the pattern in the text" $ do
         let pattern = "GCG"
         let text = "GCGCG"
         (patternCount pattern text) `shouldBe` (2 :: Int)

   describe "increment" $ do
      it "should add a new key with value 1 if the key doe not exist" $ do
         let m = M.empty
         let k = "key"
         let m2 = increment k m
         (length m2) `shouldBe` (1 :: Int)
         m2 M.! k `shouldBe` (1 :: Int)

      it "should add a increment an existing key" $ do
         let m = M.empty
         let k = "key"
         let m2 = increment k m
         let m3 = increment k m2
         let m4 = increment k m3
         (length m4) `shouldBe` (1 :: Int)
         m4 M.! k `shouldBe` (3 :: Int)

      it "should increment the correct keys" $ do
         let m = M.fromList [ ("a",10) , ("b",20) ]
         let m2 = increment "a" m
         let m3 = increment "b" m2
         let m4 = increment "c" m3
         (length m4) `shouldBe` (3 :: Int)
         m4 M.! "a" `shouldBe` (11 :: Int)
         m4 M.! "b" `shouldBe` (21 :: Int)
         m4 M.! "c" `shouldBe` (1 :: Int)


   describe "histogram" $ do
      it "should create a histogram from the input data" $ do
         let l = ["a","b","c","a","a","c","c","c"]
         let m = histogram l
         (length m) `shouldBe` (3 :: Int)
         m M.! "a" `shouldBe` (3 :: Int)
         m M.! "b" `shouldBe` (1 :: Int)
         m M.! "c" `shouldBe` (4 :: Int)

   describe "splitIntoKMers" $ do
      it "should split the text into n k-mers" $ do
         let text = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
         let k = 4
         let l = splitIntoKMers text 4
         (length l) `shouldBe` (((length text) -k +1) :: Int)
         l !! 0  `shouldBe` ("ACGT" :: [Char])

   describe "mostFrequentKMers" $ do
      it "returns the list of most frequenct k-mers in the text" $ do
         let text = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
         let k = 4
         (mostFrequentKMers k text) `shouldBe` (["CATG","GCAT"] :: [[Char]])
