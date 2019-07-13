module Main where

import           CombinatorTest
import           Test.Framework
import           Test.Framework.Providers.HUnit

main :: IO ()
main = do
  print "Testing..."
  defaultMain . hUnitTestToTests $ combinatorTests
  print "...Complete"
