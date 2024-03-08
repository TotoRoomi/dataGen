module Main (main) where


import Lib
import Data
import Generator
import Example
import Pretty
import Test.QuickCheck


main :: IO ()
main = do
  pids <- generate $ primaryKeys 1000
  uids <- generate $ primaryKeys 100
  pretty $ post pids uids
