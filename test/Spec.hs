import Test.Hspec
import Test.QuickCheck
import Populator
import Generator
import Pretty
import Data.List(notElem,nub)
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "Populator" $ do
    describe "SelfRefPairs" $ do
      keys <- runIO . generate $ primaryKeys 100
      pairs <- runIO . generate $ selfRefPairs' 100 keys
      it "non-reflexivity" $
        selfRefPairsNonReflexive keys pairs
      it "unique elements" $ do
        selfRefPairsUnique keys pairs
      it "Each key gets paired at least once" $ do
        -- This currently returns false because I haven't
        -- figured out a way to do it that works with a
        -- varying (from,to) requirement. It's always
        -- gonna be ok with (0,n) but (N,n) is not gonna work.
        selfRefPairsOccursAtLeastOnce keys pairs



selfRefPairsNonReflexive :: [PSQLTYPE] -> [[PSQLTYPE]] -> Expectation
selfRefPairsNonReflexive keys pairs = do
  let l = zip (head pairs) (head . tail $ pairs)
  let filteredL = filter (\(a,b) -> (b,a) `notElem` l) l
  length filteredL `shouldBe` length l

selfRefPairsUnique :: [PSQLTYPE] -> [[PSQLTYPE]] -> Expectation
selfRefPairsUnique keys pairs = do
   let l = zip (head pairs) (head . tail $ pairs)
   length (nub l) `shouldBe` length (head pairs)

selfRefPairsOccursAtLeastOnce :: [PSQLTYPE] -> [[PSQLTYPE]] -> Expectation
selfRefPairsOccursAtLeastOnce keys pairs = do
  let l = nub . head $ pairs
  length l `shouldBe` length keys
--pairTest = do
--  l <- generate $ primaryKeys 100
--  list <- generate $ pairs (1,10) l
--  mapM_ (\(b,c) -> putStrLn$ (showPSQLTYPE b) ++ ","++(showPSQLTYPE c))   list
--
--pairTest2 = do
--  l <- generate $ primaryKeys 100
--  list <- generate $ selfRefPairs' (10,100) l
--  putStrLn . show $ length list
--
--nonReflexivityProp :: Gen Bool
--nonReflexivityProp = do
--  l <- primaryKeys 100
--  list <- selfRefPairs' (10,100) l
--  pure $ (length list) == (length $ filter (\(a,b) -> (b,a) `notElem` list) list)
--
--pair2Test = do
--  k1 <- generate $ primaryKeys 10
--  k2 <- generate $ primaryKeys 5
--  list <- generate $ forEachKeyMakePairs' (1,10) k1 k2
--  mapM_ (\(b,c) -> putStrLn$ (showPSQLTYPE b) ++ ","++(showPSQLTYPE c))   list
--
--
--testPairs2 = do
--  k1 <- generate $ primaryKeys 10
--  k2 <- generate $ primaryKeys 5
--  ps <- generate $ forEachKeyMakePairs' (1,1) k1 k2
--  putStrLn $ "k1: " ++ (show $ length k1)
--          ++ " k2: " ++ (show $ length k2)
--          ++ " pairs: "++ (show $ length ps)
