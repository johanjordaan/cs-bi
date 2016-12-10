import Test.Hspec
import Control.Exception (evaluate)
import qualified Data.Map as M
import Lib (patternCount, increment, mostFrequentKMers)


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



   describe "mostFrequentKMers" $ do
      it "returns the list of most frequenct k-mers in the text" $ do
         let text = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
         let k = 4
         (mostFrequentKMers k text) `shouldBe` (["CATG","GCAT"] :: [[Char]])
