-- |

module Populator where
import Generator
import Test.QuickCheck
import Data.List(intercalate, nub, nubBy)
import Data.Char(toUpper)

--------------------------------------------------------------------------------
-- * PSQL Insert Statement type
--------------------------------------------------------------------------------

data InsertStatement
 = IS { schemaName :: String,
        attributes :: [String],
        values :: [PSQLTYPE]
      }
 | Statements [InsertStatement]
 deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * Insert constructors
--------------------------------------------------------------------------------

insertStatement :: String -> [String] -> [PSQLTYPE] -> InsertStatement
insertStatement s as ps = IS {schemaName = s, attributes = as, values = ps}

statements :: [InsertStatement] -> InsertStatement
statements iss = Statements iss

insert :: String -> [String] -> [[PSQLTYPE]] -> InsertStatement
insert s as pss = statements $ map (insertStatement s as) (listZip pss)

-- TODO guard and give an error if any list is of a different length
listZip :: [[PSQLTYPE]] -> [[PSQLTYPE]]
listZip l = go l [] where
  go ([]:_) acc = acc
  go l acc = go (map (drop 1) l) (map head l : acc)

--------------------------------------------------------------------------------
-- | Statement generators
--------------------------------------------------------------------------------
-- | Create non reflexive pairs from one list of keys
pairs :: (Int,Int) -> [PSQLTYPE] -> Gen [[PSQLTYPE]]
pairs ft l = do
  ns <- make (length l) $ chooseInt ft
  let ln = zip3 l ns [1,2..] -- [(PSQLTYPE,pairs to make,Index)]
  pairs <- mapM (f l) ln
  let (a,b) = unzip . concat $ pairs
  pure [a,b]
  where
    f :: [PSQLTYPE] -> (PSQLTYPE,Int,Int) -> Gen [(PSQLTYPE,PSQLTYPE)]
    f l (p, n, index)
      | drop index l == [] = pure []
      |otherwise = do
         let l' = drop index l
         ps <- make n $ do
           a <- elements l'
           pure (p,a)
         pure (nub ps)

-- | For each element in the first list,
--   choose a random element from the second list.
--   Elements from the first list occur only once,
--   but elements from the second list occur multiple times.
pairs2' :: [PSQLTYPE] -> [PSQLTYPE] -> Gen [(PSQLTYPE,PSQLTYPE)]
pairs2' k1 k2 = do
  let g = elements k2
  mapM (f g) k1
  where
    f g a = do
      b <- g
      pure (a,b)

-- | For each key in the first list choose
--   up to n random and unique keys in the second list.
pairs2 :: (Int,Int) -> [PSQLTYPE] -> [PSQLTYPE] -> Gen [[PSQLTYPE]]
pairs2 ft k1 k2 = do
  ns <- make (length k1) $ chooseInt ft
  let k1n = zip3 k1 ns [1,2..] -- [(PSQLTYPE,pairs to make,Index)]
  pairs <- mapM (f k2) k1n
  let (l1,l2) = unzip . concat $ pairs
  pure [l1,l2]
  where
    f :: [PSQLTYPE] -> (PSQLTYPE,Int,Int) -> Gen [(PSQLTYPE,PSQLTYPE)]
    f l (p, n, index) = do
      ps <- make n $ (do
           a <- elements l
           pure (p,a))
      pure (nub ps)
