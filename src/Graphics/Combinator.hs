{-# LANGUAGE MultiWayIf #-}

module Graphics.Combinator
  ( above
  , aboves
  , aboveAlign
  , Alignment
  , beside
  , besides
  , besideAlign
  , high
  , low
  , mid
  , overlay
  , overlayOffset
  , placeImage
  , placeImages
  , placeImageAlign
  , underlay
  , underlayOffset
  )
where

import           Graphics.Data.Image
import           Graphics.Util.Arithmetic
import           Prelude                 hiding ( Left
                                                , Right
                                                )

data Alignment = Low | Mid | High

low, mid, high :: Alignment
low = Low
mid = Mid
high = High

imageOffset :: (Image -> Float) -> Alignment -> Image -> Image -> Float
imageOffset dim al i1 i2 = case al of
  Low  -> lowAlign
  Mid  -> 0
  High -> negate lowAlign
  where lowAlign = (dim i2 - dim i1) / 2

above :: Image -> Image -> Image
above = aboveAlign mid

aboves :: [Image] -> Image
aboves = foldr1 above

aboveAlign :: Alignment -> Image -> Image -> Image
aboveAlign a i1 i2 = placeImage i2
                                (offset + width i1 / 2)
                                (height i1 + height i2 / 2)
                                i1
  where offset = imageOffset width a i1 i2

beside :: Image -> Image -> Image
beside = besideAlign mid

besides :: [Image] -> Image
besides = foldr1 beside

besideAlign :: Alignment -> Image -> Image -> Image
besideAlign a i1 i2 = placeImage i2
                                 (width i1 + width i2 / 2)
                                 (negate offset + height i1 / 2)
                                 i1
  where offset = imageOffset height a i1 i2

placeImage :: Image -> Float -> Float -> Image -> Image
placeImage i1 x y = placeImageAlign i1 x y mid mid

placeImages :: [Image] -> [(Float, Float)] -> Image -> Image
placeImages is ps base =
  foldr (\(i1, (x, y)) i2 -> placeImage i1 x y i2) base $ zip is ps

placeImageAlign
  :: Image -> Float -> Float -> Alignment -> Alignment -> Image -> Image
placeImageAlign i1 x y xAl yAl i2 = Image { width  = newW
                                          , height = newH
                                          , shapes = newShapes
                                          }
 where
  -- width and height of new image
  newW = max (width i2) $ max (width i1) $ if incWCase
    then (width i1 / 2) + (abs newX) + (width i2 / 2)
    else 0
  newH = max (height i2) $ max (height i1) $ if incHCase
    then (height i1 / 2) + (abs newY) + (height i2 / 2)
    else 0
  -- whether i1's width/height lay outside of i2's width/height
  incWCase = (abs newX) + (width i1 / 2) > (width i2 / 2)
  incHCase = (abs newY) + (height i1 / 2) > (height i2 / 2)
  -- x/y position converted from screen coord to cartesian coord
  newX =
    convert 0 (negate $ width i2 / 2) (width i2) (width i2 / 2) x + xOffset
  newY =
    convert 0 (height i2 / 2) (height i2) (negate $ height i2 / 2) y + yOffset
  --x/y offset based on the given alignment
  xOffset = shiftImage width xAl
  yOffset = shiftImage height yAl
  shiftImage dim a = case a of
    Low  -> dim i1 / 2
    Mid  -> 0
    High -> negate $ dim i1 / 2
  -- if i1 completely covers i2, remove i2.
  -- if i1 is within i2, don't move i2 at all, but shift i1
  -- else, move both
  newShapes
    | newW == width i1 && newH == height i1
    = shapes i1
    | newW == width i2 && newH == height i2
    = shapes i2 ++ [ (p, (ox + newX, oy + newY)) | (p, (ox, oy)) <- shapes i1 ]
    | otherwise
    = [ (p, (x2 + x2Shift, y2 + y2Shift)) | (p, (x2, y2)) <- shapes i2 ]
      ++ [ (p, (x1 + x1Shift, y1 + y1Shift)) | (p, (x1, y1)) <- shapes i1 ]
  -- shift magnitudes
  x2Shift = xDir * ((newW - width i2) / 2)
  y2Shift = yDir * ((newH - height i2) / 2)
  x1Shift = (negate xDir) * ((newW - width i1) / 2)
  y1Shift = (negate yDir) * ((newH - height i1) / 2)
  -- direction based on i1's location relative to i2
  xDir    = if
    | newX > 0  -> -1
    | newX < 0  -> 1
    | otherwise -> 0
  yDir = if
    | newY > 0  -> -1
    | newY < 0  -> 1
    | otherwise -> 0

overlay :: Image -> Image -> Image
overlay i1 i2 = placeImage i1 (width i2 / 2) (height i2 / 2) i2

--overlayAlign :: Alignment -> Alignment -> Image -> Image -> Image
--overlayAlign xAl yAl i1 = overlayAlignOffset xAl yAl i1 0 0

overlayOffset :: Image -> Float -> Float -> Image -> Image
overlayOffset = overlayAlignOffset mid mid

--overlayXY :: Image -> Float -> Float -> Image -> Image
--overlayXY i1 x y i2 = placeImage i1 (negate x) (negate y) i2

overlayAlignOffset
  :: Alignment -> Alignment -> Image -> Float -> Float -> Image -> Image
overlayAlignOffset xAl yAl i1 x y i2 =
  placeImageAlign i1 (width i2 / 2 - x) (height i2 / 2 - y) xAl yAl i2

underlay :: Image -> Image -> Image
underlay = flip overlay

--underlayAlign :: Alignment -> Alignment -> Image -> Image -> Image
--underlayAlign xAl yAl i1 i2 = overlayAlign xAl yAl i2 i1

underlayOffset :: Image -> Float -> Float -> Image -> Image
underlayOffset i1 x y i2 = overlayOffset i2 (negate x) (negate y) i1

--underlayAlignOffset
--  :: Alignment -> Alignment -> Image -> Float -> Float -> Image -> Image
--underlayAlignOffset xAl yAl i1 x y i2 = overlayAlignOffset xAl yAl i2 x y i1
--
--underlayXY :: Image -> Float -> Float -> Image -> Image
--underlayXY = underlayOffset
