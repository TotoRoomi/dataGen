-- |

module Populator where
import Generator
import Test.QuickCheck
import Data.List(intercalate, nub, nubBy)
import Data.Char(toUpper)

--------------------------------------------------------------------------------
-- * PSQL Insert Statement type
--------------------------------------------------------------------------------

-- | Represents an Insert Statement in postgreSequal
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
-- | Creates an 'InsertStatement' object based on a schemaName, list of
--   attributes and list of PSQLTYPE values for each attribute.
insertStatement :: String -> [String] -> [PSQLTYPE] -> InsertStatement
insertStatement s as ps = IS {schemaName = s, attributes = as, values = ps}

-- Not exported. Makes a collection of insert statements.
statements :: [InsertStatement] -> InsertStatement
statements iss = Statements iss

-- | Creates many 'InsertStatement' objects in a collection, using
--   the shared schemaName, shared attributes and a list of lists of values.
--   E.g. insert "user" ["name","email"] [[names],[emails]]
insert :: String -> [String] -> [[PSQLTYPE]] -> InsertStatement
insert s as pss = statements $ map (insertStatement s as) (listZip pss)

-- not exported
-- TODO guard and give an error if any list is of a different length
listZip :: [[PSQLTYPE]] -> [[PSQLTYPE]]
listZip l = go l [] where
  go ([]:_) acc = acc
  go l acc = go (map (drop 1) l) (map head l : acc)

--------------------------------------------------------------------------------
-- | Statement generators
--------------------------------------------------------------------------------
-- | Create non reflexive pairs from one list of key.
--   For each key, pair it with unique keys from the same list.
--   Each key gets a maximum nr of random pairings.
selfRefPairs :: Int -> [PSQLTYPE] -> Gen ([Int],[[PSQLTYPE]])
selfRefPairs max l = pairFactory (0,max) l l f
    where
    f :: [PSQLTYPE] -> (PSQLTYPE,Int,Int) -> Gen (Int,[(PSQLTYPE,PSQLTYPE)])
    f l (p, n, index)
      | drop index l == [] = pure (0,[])
      | otherwise = do
         let l' = drop index l
         ps <- make n $ do
           a <- elements l'
           pure (p,a)
         let ps' = nub ps
         pure (length ps', ps')

-- | Same ass 'selfRedPairs' but without the nr of pairs per element.
selfRefPairs' :: Int -> [PSQLTYPE] -> Gen [[PSQLTYPE]]
selfRefPairs' max l = do
  (_,l) <- selfRefPairs max l
  pure l

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

-- | Same as 'forEachKeyMakePairs' but discards the nr of pairs per key list.
forEachKeyMakePairs' :: (Int,Int) -> [PSQLTYPE] -> [PSQLTYPE] -> Gen [[PSQLTYPE]]
forEachKeyMakePairs' ft k1 k2 = do
  a <- forEachKeyMakePairs ft k1 k2
  pure . snd $ a

-- | For each key in the first list choose
--   up to n random and unique keys in the second list.
forEachKeyMakePairs :: (Int,Int) -> [PSQLTYPE] -> [PSQLTYPE] -> Gen ([Int],[[PSQLTYPE]])
forEachKeyMakePairs fromTo k1 k2 = pairFactory fromTo k1 k2 f
  where
    f :: [PSQLTYPE] -> (PSQLTYPE,Int,Int) -> Gen (Int,[(PSQLTYPE,PSQLTYPE)])
    f l (p, n, index) = do
      ps <- make n $ (do
           a <- elements l
           pure (p,a))
      let ps' = nub ps
      pure (length ps',ps')

-- | Make pairs based on a function that uses a list to choose from,
--   the current element to pair to, how many to pair it to and what
--   index it has.
--   Returns the number of pairs per element from the first list
--   and and two lists of an unziped pair in a list.
--   ([nr of pairs per element],[[p1],[p2]])
pairFactory :: (Int,Int) -> [PSQLTYPE] -> [PSQLTYPE]
               -> ([PSQLTYPE] -> (PSQLTYPE,Int,Int)
                   -> Gen (Int,[(PSQLTYPE,PSQLTYPE)])
                  )
               -> Gen ([Int],[[PSQLTYPE]])
pairFactory fromTo k1 k2 f = do
  ns <- make (length k1) $ chooseInt fromTo
  let k1n = zip3 k1 ns [1,2..] -- [(PSQLTYPE,pairs to make,Index)]
  nrAndPairs <- mapM (f k2) k1n -- [(Int,[(p,p)])]
  let (nrs,pairs) = unzip nrAndPairs
  let (l1,l2) = unzip . concat $ pairs
  pure (nrs,[l1,l2])

-- | For each date in the list of dates, make a date that is equal to
--   or later than that date.
forEachDateMakeDates :: [PSQLTYPE] -> [Int] -> Gen [[PSQLTYPE]]
forEachDateMakeDates ds i = do
  let fromDates = zip i (map unDate ds)
  mapM makeToDates fromDates
  where
    makeToDates :: (Int,(Int,Int,Int)) -> Gen [PSQLTYPE]
    makeToDates (i,(fy,fm,fd)) = make i $ dateBetween (fy,fm,fd) (fy+1,1,1)
