module Graphics.Combinator
  ( above
  , aboveAlign
  , Alignment
  , beside
  , besideAlign
  , high
  , low
  , mid
  , placeImage
  , placeImageAlign
  , overlay
  , underlay
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
  Low  -> (negate $ (dim i1) - (dim i2)) / 2
  Mid  -> 0
  High -> ((dim i1) - (dim i2)) / 2

above :: Image -> Image -> Image
above = aboveAlign mid

aboveAlign :: Alignment -> Image -> Image -> Image
aboveAlign a i1 i2 = Image
  { width  = max (width i1) (width i2)
  , height = (height i1) + (height i2)
  , shapes = [ (p, (x, y + (height i2 / 2))) | (p, (x, y)) <- shapes i1 ]
               ++ [ (p, (x + offset, y - (height i1 / 2)))
                  | (p, (x, y)) <- shapes i2
                  ]
  }
  where offset = imageOffset width a i1 i2

beside :: Image -> Image -> Image
beside = besideAlign mid

besideAlign :: Alignment -> Image -> Image -> Image
besideAlign a i1 i2 = Image
  { width  = (width i1) + (width i2)
  , height = max (height i1) (height i2)
  , shapes = [ (p, (x - (width i2 / 2), y)) | (p, (x, y)) <- shapes i1 ]
               ++ [ (p, (x + (width i1 / 2), y + offset))
                  | (p, (x, y)) <- shapes i2
                  ]
  }
  where offset = imageOffset height a i1 i2

placeImage :: Image -> Float -> Float -> Image -> Image
placeImage i1 x y i2 = placeImageAlign i1 x y mid mid i2

placeImageAlign
  :: Image -> Float -> Float -> Alignment -> Alignment -> Image -> Image
placeImageAlign i1 x y xAl yAl i2 = Image
  { width  = width i2 -- Assuming that i2 is base (just like in htdp/image)
  , height = height i2
  , shapes = shapes i2
               ++ [ (p, (ox + newX + xOffset, oy + newY + yOffset))
                  | (p, (ox, oy)) <- shapes i1
                  ]
  }
 where
  newX    = convert 0 (negate $ (width i2) / 2) (width i2) ((width i2) / 2) x
  newY    = convert 0 ((height i2) / 2) (height i2) (negate $ (height i2) / 2) y
  xOffset = shiftImage width xAl
  yOffset = shiftImage height yAl
  shiftImage dim a = case a of
    Low  -> (dim i1) / 2
    Mid  -> 0
    High -> negate $ (dim i1) / 2

overlay :: Image -> Image -> Image
overlay i1 i2 = placeImage i1 ((width i2) / 2) ((height i2) / 2) i2

underlay :: Image -> Image -> Image
underlay = flip overlay
