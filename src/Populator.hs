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

pairs :: (Int,Int) -> [PSQLTYPE] -> Gen [[(PSQLTYPE,PSQLTYPE)]]
pairs ft l = do
  ns <- make (length l) $ chooseInt ft
  let ln = zip3 l ns [1,2..] -- [(PSQLTYPE,pairs to make,Index)]
  mapM (f l) ln
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

--nonReflexivePairs :: (Int, Int) -> [PSQLTYPE] -> [PSQLTYPE] -> Gen [(PSQLTYPE,PSQLTYPE)]
nonReflexivePairs = undefined
  -- for each a build a list from the rest of the lis

-- -- | Create the final insert statement
-- insertStatement :: SchemaName -> [Atributes] -> [Values] -> InsertStatement
-- insertStatement schemaName as vs =
--   "INSERT INTO "++(map toUpper schemaName)
--   ++ listify as ++ "\n"
--   ++ "VALUES " ++ listify vs ++ ";\n"
--   where
--     listify as = "("++ intercalate "," as ++ ")"

-- * insert constructors

---- | Produces Friends without any duplicates
--insertFriend n = do
--  userIDs <- primaryKeys n
--  -- for each userID pick n random other userIDs
--  -- for each list of friends make an insertStatement with p1 p2, p1 p3
--  makeForAll userIDs
--  where
--    -- listOfFriends :: [Gen [uIDs]]
--    listsOfFriends userIDs = map (pickIds userIDs) userIDs
--    pickIds userIDs uid= do
--      i <- chooseInt (0,n)
--      f <- gen i (elements userIDs)
--      let friends = filter (\a -> a/=uid) f
--      pure friends
--    personAndFriends userIDs = zip userIDs $ listsOfFriends userIDs -- [(uid,Gen [uids])]
--    makeForAll userIDs = do
--      -- make a set of all the friend pairs then delete any duplicates where (1,2) and (2,1) are duplicates
--      --                                  [Gen [(Int,Int)]]    [(Int, Gen [Int])]
--      l <- cleanup $ map (makePairs) $ personAndFriends userIDs -- [Gen [uid,fid]]
--      pure $ map (ms) l
--    makeStatements :: Gen [(Int,Int)] -> Gen [String]
--    makeStatements (gpl) = do
--      pairList <- gpl -- [(uid,fid)]
--      pure $ map (ms) pairList
--    ms (uid,fid) = do
--      insertStatement "Friend" ["UserID","FriendID"] [show uid, show fid]
--    makePairs :: (Int, Gen [Int]) -> Gen [(Int,Int)]
--    makePairs (uid, glist) = do
--      friends <- glist
--      pure $ map ((\uid fid -> (uid,fid) ) uid ) friends -- Gen [(uid,fid)]
--    cleanup :: [Gen [(Int,Int)]] -> Gen [(Int,Int)]
--    cleanup listG = do
--      list <- sequence listG -- Gen [[(Int,Int)]]
--      pure $ nubBy (\(a,b) (c,d) -> a==d && b == c )  $ concat list
