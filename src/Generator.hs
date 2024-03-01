-- |  Generates usable data for insert statements
module Generator where

import Data
import Test.QuickCheck
import Data.List(intercalate, nub, nubBy)
import Data.Char(toUpper)


--------------------------------------------------------------------------------
-- * PSQL types
--------------------------------------------------------------------------------

data PSQLTYPE
  = VARCHAR String
  | DATE (Int, Int, Int) -- year, month, day
  | INTEGER Int
  deriving (Eq,Show)

data InsertStatement
 = IS { schemaName :: String,
        attributes :: [String],
        values :: [PSQLTYPE]
      }
 | Statements [InsertStatement]
 deriving (Eq, Show)

--------------------------------------------------------------------------------
-- * PSQL constructors
--------------------------------------------------------------------------------

psqlVarchar :: String -> PSQLTYPE
psqlVarchar s = VARCHAR s

psqlDate :: Int -> Int -> Int -> PSQLTYPE
psqlDate y m d = DATE (y,m,d)

psqlInteger :: Int -> PSQLTYPE
psqlInteger n = INTEGER n

insertStatement :: String -> [String] -> [PSQLTYPE] -> InsertStatement
insertStatement s as ps = IS {schemaName = s, attributes = as, values = ps}

statements :: [InsertStatement] -> InsertStatement
statements iss = Statements iss
--------------------------------------------------------------------------------
-- * Process PSQLTYPE Data
--------------------------------------------------------------------------------

genPSQLTYPEList :: Int -> (a -> PSQLTYPE) -> [a] -> Gen [PSQLTYPE]
genPSQLTYPEList n f l = gen n $ elements $ map f l

firstnames :: Int -> Gen [PSQLTYPE]
firstnames n = genPSQLTYPEList n psqlVarchar firstname

lastnames :: Int -> Gen [PSQLTYPE]
lastnames n =  genPSQLTYPEList n psqlVarchar surname

psqlName :: PSQLTYPE -> PSQLTYPE -> PSQLTYPE
psqlName (VARCHAR first) (VARCHAR last) = VARCHAR (first ++ " " ++ last)

--------------------------------------------------------------------------------
-- * Attribute generators and helpers
--------------------------------------------------------------------------------

-- | generates a random full name
name :: Gen PSQLTYPE
name = do
  fn <- elements firstname
  ln <- elements surname
  pure . psqlVarchar $ fn ++ "" ++ ln

-- | Generates a full name using the given first and lastname
name2 :: PSQLTYPE -> PSQLTYPE -> PSQLTYPE
name2 (VARCHAR fn) (VARCHAR sn) = VARCHAR $  fn ++ " " ++ sn

-- | Generates a random email
email :: Gen PSQLTYPE
email = do
  fn <- elements $ firstname
  sn <- elements $ surname
  n <- num
  k <- num
  pure . psqlVarchar $ (take n fn)++(take k sn)++emaildomain
  where
    num = chooseInt (1,5)

-- | Generates an email with the specified first and lastname
email2 :: PSQLTYPE -> PSQLTYPE -> Gen PSQLTYPE
email2 (VARCHAR fn) (VARCHAR sn) = do
  n <- num
  k <- num
  pure . psqlVarchar $ (take n fn)++(take k sn)++emaildomain
  where
    num = chooseInt (1,5)

-- | Creates a date with the specified year
date :: Int -> Gen PSQLTYPE
date year = do
  month <- chooseInt (1,12)
  day <- dayGen month
  pure $ psqlDate year month day
  where
    dayGen m | m == 2 = chooseInt (1,28)
             | m == 4 || m == 6 || m == 9 || m == 11
             = chooseInt (1,30)
             | otherwise = chooseInt (1,31)

-- | Produces a list of unique primary keys at the specified length
--   Obs cannot generate more than 88888 numbers
primaryKeys :: Int -> Gen [PSQLTYPE]
primaryKeys n = do
  if n > 88888 then pure $ fail "Cannot generate more than upper bound 88 888 numbers" -- this just makes an empty list..
    else loop $ numlist n
  where
    loop gl = do
      l <- gl
      let ls = nub l
      if length ls < n then
        loop $ increase (n - length ls) ls
        else pure $ map psqlInteger ls
    increase i l = do
      ns <- numlist i
      pure $ l ++ (nub ns)
    numlist n = vectorOf n (chooseInt (11111,99999)) -- 88 888 unique numbers



--------------------------------------------------------------------------------
-- | Statement generators
--------------------------------------------------------------------------------


-- | Make an n long list of randomly generated items
gen :: Int -> Gen a -> Gen [a]
gen 0 g = pure []
gen n g = do
  a <- g
  b <- gen (n - 1) g
  pure (a : b)

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


      

--test n = do a <-generate $  insertUsers n; mapM_ (putStrLn) a
--test2 = do a<- generate $ insertUser; putStrLn a
--testFriend n= do a <- generate$ insertFriend n; mapM_ ( putStrLn) a
