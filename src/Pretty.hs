-- |
module Pretty where
import Generator
import Populator
import Test.QuickCheck
import Data.List(intercalate)
import Data.Char(toUpper)



-- | Pretty print a Gen a to see the a
pretty :: Gen InsertStatement -> IO ()
pretty gs = do
  s <- generate gs
  case s of
    IS _ _ _ -> putStrLn $ prettyPrintIS s
    Statements ss ->mapM_ putStrLn $ map prettyPrintIS ss

prettyPrintIS :: InsertStatement -> String
prettyPrintIS (IS schemaName as vs) =
    "INSERT INTO "++(map toUpper schemaName)
  ++ listify as ++ "\n"
  ++ "VALUES " ++ listify (map showPSQLTYPE vs) ++ ";\n"
  where
    listify as = "("++ intercalate "," as ++ ")"

debugPSQLList i g = do l <- generate $ g i; mapM_ (putStrLn . showPSQLTYPE) l
debugPSQL g = do a <- generate g; putStrLn $ showPSQLTYPE a
