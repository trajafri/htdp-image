module CombinatorTest
  ( combinatorTests
  )
where

import           Graphics.Data.Image
import           Graphics.Htdp
import           Graphics.Gloss                 ( Picture )
import           Graphics.Gloss.Data.Picture    ( blank )
import           Test.HUnit

bp :: Picture
bp = blank -- blank picture

image1 :: Image
image1 = Image { width  = 50
               , height = 50
               , shapes = [(bp, (0, 0)), (bp, (10, 10)), (bp, (30, 20))]
               }

image2 :: Image
image2 = Image { width  = 10
               , height = 50
               , shapes = [(bp, (30, 20)), (bp, (10, 40)), (bp, (70, 90))]
               }

image3 :: Image
image3 = Image { width  = 50
               , height = 10
               , shapes = [(bp, (0, 20)), (bp, (10, 0)), (bp, (7, 9))]
               }

widthError :: String
widthError = "creates incorrect width for images"

heightError :: String
heightError = "creates incorrect height for images"

posError :: String
posError = "shifts images incorrectly"

 -- Check if size increases properly
aboveWidthTest :: Test
aboveWidthTest = TestCase $ do
  assertBool widthError (50 == (width $ above image1 image2))
  assertBool widthError (50 == (width $ above image1 image3))
  assertBool widthError (50 == (width $ above image2 image3))
  assertBool widthError (10 == (width $ above image2 image2))

-- Check if height is the max of two
aboveHeightTest :: Test
aboveHeightTest = TestCase $ do
  assertBool heightError (100 == (height $ above image1 image2))
  assertBool heightError (60 == (height $ above image1 image3))
  assertBool heightError (60 == (height $ above image2 image3))

-- Check if positions are shifted correctly
abovePosTest :: Test
abovePosTest = TestCase $ do
  assertBool
    posError
    (  [(0, 25), (10, 35), (30, 45), (30, -5), (10, 15), (70, 65)]
    == (map snd . shapes $ above image1 image2)
    )
  assertBool
    posError
    (  [(0, 5), (10, 15), (30, 25), (0, -5), (10, -25), (7, -16)]
    == (map snd . shapes $ above image1 image3)
    )
  assertBool
    posError
    (  [(30, 25), (10, 45), (70, 95), (0, -5), (10, -25), (7, -16)]
    == (map snd . shapes $ above image2 image3)
    )

-- Check if size increases properly
besideWidthTest :: Test
besideWidthTest = TestCase $ do
  assertBool widthError (60 == (width $ beside image1 image2))
  assertBool widthError (100 == (width $ beside image1 image3))
  assertBool widthError (60 == (width $ beside image2 image3))

-- Check if height is the max of two
besideHeightTest :: Test
besideHeightTest = TestCase $ do
  assertBool heightError (50 == (height $ beside image1 image2))
  assertBool heightError (50 == (height $ beside image1 image3))
  assertBool heightError (50 == (height $ beside image2 image3))
  assertBool heightError (10 == (height $ beside image3 image3))

-- Check if positions are shifted correctly
besidePosTest :: Test
besidePosTest = TestCase $ do
  assertBool
    posError
    (  [(-5, 0), (5, 10), (25, 20), (55, 20), (35, 40), (95, 90)]
    == (map snd . shapes $ beside image1 image2)
    )
  assertBool
    posError
    (  [(-25, 0), (-15, 10), (5, 20), (25, 20), (35, 0), (32, 9)]
    == (map snd . shapes $ beside image1 image3)
    )
  assertBool
    posError
    (  [(5, 20), (-15, 40), (45, 90), (5, 20), (15, 0), (12, 9)]
    == (map snd . shapes $ beside image2 image3)
    )

-- Location doesn't matter since the image being placed on determines
-- the size
placeImageSizeTest :: Test
placeImageSizeTest = TestCase $ sequence_
  [ assertBool
      "dimensions computed incorrectly"
      (  (height i2)
      == (height $ placeImage i1 0 0 i2)
      && (width i2)
      == (width $ placeImage i1 0 0 i2)
      )
  | i1 <- [image1, image2, image3]
  , i2 <- [image1, image2, image3]
  ]

placeImagePosTest :: Test
placeImagePosTest = TestCase $ sequence_
  [ assertBool
      posError
      (  (map (\(p, (x, y)) -> (p, (x + 20, y + 50))) . shapes $ i1)
      == (drop (length . shapes $ i2) $ shapes $ placeImage i1 20 50 i2)
      )
  | i1 <- [image1, image2, image3]
  , i2 <- [image1, image2, image3]
  ]

combinatorTests :: Test
combinatorTests = TestList
  [ TestLabel "aboveWidthTest"     aboveWidthTest
  , TestLabel "aboveHeightTest"    aboveHeightTest
  , TestLabel "abovePosTest"       abovePosTest
  , TestLabel "besideWidthTest"    besideWidthTest
  , TestLabel "besideHeightTest"   besideHeightTest
  , TestLabel "besidePosTest"      besidePosTest
  , TestLabel "placeImageSizeTest" placeImageSizeTest
  , TestLabel "placeImagePosTest"  placeImagePosTest
  ]
