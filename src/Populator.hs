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
--------------------------------------------------------------------------------
-- | Statement generators
--------------------------------------------------------------------------------


-- -- | Create the final insert statement
-- insertStatement :: SchemaName -> [Atributes] -> [Values] -> InsertStatement
-- insertStatement schemaName as vs =
--   "INSERT INTO "++(map toUpper schemaName)
--   ++ listify as ++ "\n"
--   ++ "VALUES " ++ listify vs ++ ";\n"
--   where
--     listify as = "("++ intercalate "," as ++ ")"

-- * insert constructors


insertUser :: Gen InsertStatement
insertUser = do
  fn <- firstnames 1
  sn <- lastnames 1
  let name = name2 (head fn) (head sn)
  email <- email2 (head fn) (head sn)
  date <- date 2024
  pure $ insertStatement "user" ["name","email","joinDate"] [name,email,date]

insertUsers :: Int -> Gen InsertStatement
insertUsers n = do
  primarykeys <- primaryKeys n
  fns <- firstnames n
  lns <- lastnames n
  pure $ statements $ map makeUser (zip3 primarykeys fns lns)
  where
    makeUser (pm,fn,sn) =
      insertStatement "user" ["userId","name"] [pm,name2 fn sn]

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
