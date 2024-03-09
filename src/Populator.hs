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
insert s as pss = statements $ map (insertStatement s as) pss
--------------------------------------------------------------------------------
-- | Statement generators
--------------------------------------------------------------------------------

pairs :: (Int,Int) -> [PSQLTYPE] -> Gen [(PSQLTYPE,PSQLTYPE)]
pairs ft l = do
  ns <- make (length l) $ chooseInt ft
  let ln = zip3 l ns [1,2..] -- [(PSQLTYPE,pairs to make,Index)]
  pairs <- mapM (f l) ln
  pure . concat $ pairs
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

reflexivePairs = undefined
