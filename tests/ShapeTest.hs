module ShapeTest
  ( shapeTests
  )
where

import           Graphics.Htdp.Data.Image
import           Graphics.Htdp
import qualified Graphics.Gloss                as G
import           Test.HUnit

errorMsg :: String
errorMsg = "drawn incorrectly"

circleTests :: Test
circleTests = TestCase $ do
  assertEqual errorMsg
              (G.Color green $ G.circleSolid 10)
              (fst . head . shapes $ circle 10 solid green)
  assertEqual errorMsg
              (G.Color green $ G.circle 10)
              (fst . head . shapes $ circle 10 outline green)

shapeTests :: Test
shapeTests = TestList [TestLabel "circleTests" circleTests]

