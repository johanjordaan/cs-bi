import Test.Hspec
import Control.Exception (evaluate)
import Lib (patternCount, mostFrequentKMers)


main :: IO ()
main = hspec $ do
  describe "patterCount" $ do
    it "returns the number of occurences of the pattern in the text" $ do
      let pattern = "GCG"
      let text = "GCGCG"
      (patternCount pattern text) `shouldBe` (2 :: Int)

  describe "mostFrequentKMers" $ do
    it "returns the list of most frequenct k-mers in the text" $ do
      let text = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
      let k = 4
      (mostFrequentKMers k text) `shouldBe` (["CATG","GCAT"] :: [[Char]])
