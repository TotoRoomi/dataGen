-- |  Generates usable data for insert statements
module Generator where

import Data
import Test.QuickCheck
import Data.List(intercalate)
import Data.Char(toUpper)

name :: Gen String -> Gen String -> Gen String
name (gfn) (gsn) = do
  fn <- gfn
  sn <- gsn
  pure $ fn ++ " " ++ sn

name2 :: String -> String -> String
name2 fn sn =  fn ++ " " ++ sn


email :: Gen String -> Gen String -> Gen String
email gfn gsn = do
  fn <- gfn
  sn <- gsn
  n <- num
  k <- num
  pure $ (take n fn)++(take k sn)++emaildomain
  where
    num = chooseInt (1,5)

email2 :: String -> String -> Gen String
email2 fn sn = do
  n <- num
  k <- num
  pure $ (take n fn)++(take k sn)++emaildomain
  where
    num = chooseInt (1,5)

date :: String -> Gen String
date year = do
  month <- chooseInt (1,12)
  day <- dayGen month
  pure $ year+-(show month)+-(show day)
  where
    dayGen m | m == 2 = chooseInt (1,28)
             | m == 4 || m == 6 || m == 9 || m == 11
             = chooseInt (1,30)
             | otherwise = chooseInt (1,31)
    (+-) :: String -> String -> String
    (+-) a b = a ++ "-" ++ b

insertUser :: Gen String
insertUser = do
  fn <- firstname
  sn <- surname
  let name = name2 fn sn
  email <- email2 fn sn
  date <- date "2024"
  pure $ insertStatement "user" ["name","email","joinDate"] [name,email,date]


type SchemaName = String
type Atributes = String
type Values = String
insertStatement :: String -> [Atributes] -> [Values] -> String
insertStatement schemaName as vs =
  "INSERT INTO "++(map toUpper schemaName)
  ++ listify as ++ "\n"
  ++ "VALUES " ++ listify vs ++ ";\n"
  where
    listify as = "("++ intercalate "," as ++ ")"
