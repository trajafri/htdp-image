module Main where

import           CombinatorTest
import           ShapeTest
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

main :: IO ()
main = do
  print "Testing..."
  defaultMain . hUnitTestToTests $ TestList [combinatorTests, shapeTests]
  print "...Complete"
