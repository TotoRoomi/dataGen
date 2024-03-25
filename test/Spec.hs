import Test.Hspec
import Test.QuickCheck
import Populator
import Generator
import Pretty
import Data.List(notElem,nub, findIndices)
import Control.Exception (evaluate)
import Data.Time.Calendar

main :: IO ()
main = hspec $ do
  describe "Populator" $ do
    keys <- runIO . generate $ primaryKeys 100

    describe "SelfRefPairs" $ do
      pairs <- runIO . generate $ selfRefPairs' 100 keys

      it "non-reflexivity" $
        selfRefPairsNonReflexive keys pairs

      it "unique elements" $ do
        selfRefPairsUnique keys pairs

      it "no key gets more than the max nr of pairings" $ do
        selfRefPairsMaxPairings keys pairs 100

    describe "forEachKeyMakePairs" $ do
      let k1 = take 50 keys
      let k2 = drop 50 keys
      (ns, pairs) <- runIO . generate $ forEachKeyMakePairs (1,10) k1 k2

      it "max n keypairs per key" $
        forEachKeyMaxPairs ns 10

      it "unique keys for every key" $
        forEachKeyUniqueKeys pairs

    describe "forEachDateMakeDates" $ do
      seedDates <- runIO $ generate $ make 1000 $ dateBetween (1995,6,18) (2024,6,18)
      datesPerSeed <- runIO $ generate $ make 1000 $ chooseInt (1,1000)
      dates <- runIO $ generate $ forEachDateMakeDates seedDates datesPerSeed

      it "makes dates that are later than seed dates" $
        makeDatesLaterThanSeed seedDates dates

      it "makes n dates per seed" $
        makeNDatesPerSeed dates datesPerSeed



------------- Helpers ----------------
selfRefPairsNonReflexive :: [PSQLTYPE] -> [[PSQLTYPE]] -> Expectation
selfRefPairsNonReflexive keys pairs = do
  let l = zip (head pairs) (head . tail $ pairs)
  let filteredL = filter (\(a,b) -> (b,a) `notElem` l) l
  length filteredL `shouldBe` length l

selfRefPairsUnique :: [PSQLTYPE] -> [[PSQLTYPE]] -> Expectation
selfRefPairsUnique keys pairs = do
   let l = zip (head pairs) (head . tail $ pairs)
   length (nub l) `shouldBe` length (head pairs)

selfRefPairsMaxPairings :: [PSQLTYPE] -> [[PSQLTYPE]] -> Int -> Expectation
selfRefPairsMaxPairings keys pairs max = do
  let p1 = head pairs
  let p2 = head . tail $ pairs
  let t1 = and $ map (occurs max p1) keys
  let t2 = and $ map (occurs max p2) keys
  (t1,t2) `shouldSatisfy` (\x -> x == (True,True))
  where
    occurs max list elem = (length $ findIndices (\a-> a == elem) list) <= max

forEachKeyMaxPairs :: [Int] -> Int -> Expectation
forEachKeyMaxPairs ns max = do
  ns `shouldSatisfy` (\ns -> and $ map (\a-> a <= max && a >= 1) ns)


forEachKeyUniqueKeys :: [[PSQLTYPE]] -> Expectation
forEachKeyUniqueKeys pairs = do
  let p1 = head pairs
  let p2 = head . tail $ pairs
  let zipped = zip p1 p2
  length (nub zipped) `shouldBe` length p1

makeDatesLaterThanSeed :: [PSQLTYPE] -> [[PSQLTYPE]] -> Expectation
makeDatesLaterThanSeed sds ds = do
  let seedDates = map (toDay . unDate) sds
  let dates = map (map (toDay . unDate)) ds
  let zipped = zip seedDates dates
  zipped `shouldSatisfy` (checkDates)
  where
    toDay (y,m,d) = fromGregorian (toInteger y) m d
    checkDates z = and $ map go z
    go (seed, dates) = and $ map (\d -> seed <= d) dates

makeNDatesPerSeed :: [[PSQLTYPE]] -> [Int] -> Expectation
makeNDatesPerSeed dates datesPerSeed = do
  let l = zip datesPerSeed $ map (length) dates
  l `shouldSatisfy` checkList
  where
    checkList l = and $ map (\(expected,actual)-> expected == actual) l
