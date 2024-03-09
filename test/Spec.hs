import Test.QuickCheck
import Populator
import Generator
import Pretty
import Example
import Data.List(notElem)


main :: IO ()
main = putStrLn "Test suite not yet implemented"


pairTest = do
  l <- generate $ primaryKeys 100
  list <- generate $ pairs (1,10) l
  mapM_ (\(b,c) -> putStrLn$ (showPSQLTYPE b) ++ ","++(showPSQLTYPE c))   list

pairTest2 = do
  l <- generate $ primaryKeys 100
  list <- generate $ pairs (10,100) l
  putStrLn . show $ length list

nonReflexivityProp :: Gen Bool
nonReflexivityProp = do
  l <- primaryKeys 100
  list <- pairs (10,100) l
  pure $ (length list) == (length $ filter (\(a,b) -> (b,a) `notElem` list) list)

testFriend = do
  ks <- generate $ primaryKeys 5
  pretty $ friend ks
