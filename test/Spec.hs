import Test.Hspec
import Control.Exception (evaluate)
import qualified Data.Map as M
import Lib
         (
            patternCount,
            increment,
            histogram,
            splitIntoKMers,
            kMersHistogram,
            histogramMax,
            mostFrequentKMers,
            frequentKMers,
            compliment,
            reverseCompliment,
            patternToNumber,
            numberToPattern,
            frequencyArray,
            patternPositions,
            findClumps,
            skew,
            minSkewIndices,
            hammingDistance,
            approximatePatternPositions,
            approximatePatternCount,
            neighbors
         )


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

  describe "kMersHistogram" $ do
    it "should construct a histogram of the k-mers in the text" $ do
      let text = "AAAABBBAAA"
      let k = 3
      let m = kMersHistogram text k
      (length m) `shouldBe` (6 :: Int)
      m M.! "AAA" `shouldBe` (3 :: Int)
      m M.! "BBB" `shouldBe` (1 :: Int)

  describe "histogramMax" $ do
    it "should return the histogram items with the highest frequency" $ do
      let text = "AAAABBBAAA"
      let k = 3
      let m = histogramMax $ kMersHistogram text k
      m `shouldBe` (3 :: Int)

  describe "frequentKMers" $ do
    it "returns the list of k-mers with a frequency bigger then t" $ do
      let text = "aactctatacctcctttttgtcgaatttgtgtgatttatagagaaaatcttattaactgaaactaaaatggtaggtttggtggtaggttttgtgtacattttgtagtatctgatttttaattacataccgtatattgtattaaattgacgaacaattgcatggaattgaatatatgcaaaacaaacctaccaccaaactctgtattgaccattttaggacaacttcagggtggtaggtttctgaagctctcatcaatagactattttagtctttacaaacaatattaccgttcagattcaagattctacaacgctgttttaatgggcgttgcagaaaacttaccacctaaaatccagtatccaagccgatttcagagaaacctaccacttacctaccacttacctaccacccgggtggtaagttgcagacattattaaaaacctcatcagaagcttgttcaaaaatttcaatactcgaaacctaccacctgcgtcccctattatttactactactaataatagcagtataattgatctga"
      let k = 9
      let t = 3
      let mfk = frequentKMers text k t
      mfk `shouldBe` (["aaacctacc","aacctacca","acctaccac","cctaccacc","ggtaggttt","tggtaggtt"] :: [[Char]])

  describe "mostFrequentKMers" $ do
    it "returns the list of most frequenct k-mers in the text" $ do
      let text = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
      let k = 4
      let mfk = mostFrequentKMers text k
      mfk `shouldBe` (["CATG","GCAT"] :: [[Char]])

  describe "compliment" $ do
    it "should return the compliment of the sequence" $ do
      let s = "TTGTGTC"
      let rc = compliment s
      rc `shouldBe` ("AACACAG" :: [Char])

  describe "reverseCompliment" $ do
    it "should return the reverse compliment of the sequence" $ do
      let s = "TTGTGTC"
      let rc = reverseCompliment s
      rc `shouldBe` ("GACACAA" :: [Char])

  describe "patternToNumber" $ do
    it "should return the number for the pattern" $ do
      let p = "ATGCAA"
      let n = patternToNumber p
      n `shouldBe` (912 :: Int)

  describe "numberToPattern" $ do
    it "should return the pattern given the number and k" $ do
      let n = 912
      let k = 6
      let p = numberToPattern n k
      p `shouldBe` ("ATGCAA" :: [Char])

  describe "frequencyArray" $ do
    it "should return a frequency arry for the kmers" $ do
      let t = "ACGCGGCTCTGAAA"
      let k = 2
      let fa = frequencyArray t k
      (length fa) `shouldBe` (16 :: Int)
      (fa !! 0) `shouldBe` (2 :: Int)

  describe "patternPositions" $ do
    it "should return an array with the staring positions of the given pattern" $ do
      let t = "GATATATGCATATACTT"
      let p = "ATAT"
      let pp = patternPositions t p
      (length pp) `shouldBe` (3 :: Int)
      (pp !! 0) `shouldBe` (1 :: Int)
      (pp !! 1) `shouldBe` (3 :: Int)
      (pp !! 2) `shouldBe` (9 :: Int)

  describe "findClumps" $ do
    it "should find the k-mer clumps in the text using L window length and t frequecy" $ do
      let text = "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGAGAAGAGGAAACATTGTAA"
      let k = 5
      let l = 50
      let t = 4
      let c = findClumps text k l t
      (length c) `shouldBe` (2 :: Int)
      c `shouldBe` (["CGACA","GAAGA"] :: [[Char]])

    it "should process a sizable file and produce the clumps array" $ do
      text <- readFile "./test/fixtures/clump_finding"
      let k = 11
      let l = 566
      let t = 18
      let c = findClumps text k l t
      (length c) `shouldBe` (1 :: Int)
      c `shouldBe` (["AAACCAGGTGG"] :: [[Char]])

  describe "skew" $ do
    it "should return the skew of a small genome" $ do
      let t = "CATGGGCATCGGCCATACGCC"
      let s = skew t
      s `shouldBe` ([0,-1,-1,-1,0,1,2,1,1,1,0,1,2,1,0,0,0,0,-1,0,-1,-2] :: [Int])

    it "should return the skew of a larger genome" $ do
      t <- readFile "./test/fixtures/skew"
      let s = skew t
      (length s) `shouldBe` ((length t)+1  :: Int)

  describe "minSkewIndices" $ do
    it "should return the minimum skew indexes a small genome" $ do
      let t = "TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT"
      let msi = minSkewIndices t
      msi `shouldBe` ([11,24] :: [Int])

    it "should return the minimum skew indexes a larger genome" $ do
      t <- readFile "./test/fixtures/skew"
      let msi = minSkewIndices t
      msi `shouldBe` ([17462,17463,17464] :: [Int])

  describe "hammingDistance" $ do
    it "should return the hamming distance between two strings" $ do
      let a = "GGGCCGTTGGT"
      let b = "GGACCGTTGAC"
      let h = hammingDistance a b
      h `shouldBe` (3 :: Int)

  describe "approximatePatternPositions" $ do
    it "should return the approximate (hamming distance d) pattern matches in the genome" $ do
      let p = "ATTCTGGA"
      let t = "CGCCCGAATCCAGAACGCATTCCCATATTTCGGGACCACTGGCCTCCACGGTACGGACGTCAATCAAAT"
      let d = 3
      let app = approximatePatternPositions p t d
      app `shouldBe` ([6,7,26,27] :: [Int])

  describe "approximatePatternCount" $ do
    it "should return the number of approximate (hamming distance d) pattern matches in the genome" $ do
      let p = "ATTCTGGA"
      let t = "CGCCCGAATCCAGAACGCATTCCCATATTTCGGGACCACTGGCCTCCACGGTACGGACGTCAATCAAAT"
      let d = 3
      let apc = approximatePatternCount p t d
      apc `shouldBe` (4 :: Int)

  decribe "neighbors" $ do
    it "should return the d neighbors of pattern" $ do
      let p = "ACG"
      let d = 1
      let n = neighbors p d
      n `shouldBe` (["CCG","TCG","GCG","AAG","ATG","AGG","ACA","ACC","ACT","ACG"] :: [[Char]])
