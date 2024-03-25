-- |
module Pretty
  (pretty
  ,debugPSQLList
  ,debugPSQL
  ,debugPSQL'
  )

where
import Generator
import Populator
import Test.QuickCheck
import Data.List(intercalate)
import Data.Char(toUpper)



-- | Pretty print a Gen InsertStatement in the PSQL format
--   of insert statements. Can either take a collection
--   of InsertStatement or a single one.
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

-- | Takes an n and a list generator of random PSQLTYPE data and prints it
--   in a readible format for debuging.
debugPSQLList i g = do l <- generate $ g i; mapM_ (putStrLn . showPSQLTYPE) l

-- | Takes a generator and prints it in a readible format for debuging
debugPSQL g = do a <- generate g; putStrLn $ showPSQLTYPE a

-- | Takes a generator and the amount of data to be generated,
--   prints that amount in a readible format for debuging.
debugPSQL' i g = do l <- generate $ make i g; mapM_ (putStrLn . showPSQLTYPE) l
