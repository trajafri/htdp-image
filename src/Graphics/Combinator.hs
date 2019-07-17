module Graphics.Combinator
  ( above
  , beside
  , placeImage
  , overlay
  , underlay
  )
where

import           Graphics.Data.Image
import           Graphics.Util.Arithmetic

above :: Image -> Image -> Image
above i1 i2 = Image
  { width  = max (width i1) (width i2)
  , height = (height i1) + (height i2)
--                                 Gloss' y seems to be positive upwards :/
  , shapes = (map (\(p, (x, y)) -> (p, (x, y + (height i2 / 2)))) $ shapes i1)
               ++ ( map (\(p, (x, y)) -> (p, (x, y - (height i1 / 2))))
                  $ shapes i2
                  )
  }

beside :: Image -> Image -> Image
beside i1 i2 = Image
  { width  = (width i1) + (width i2)
  , height = max (height i1) (height i2)
  , shapes = (map (\(p, (x, y)) -> (p, (x - (width i2 / 2), y))) $ shapes i1)
               ++ ( map (\(p, (x, y)) -> (p, (x + (width i1 / 2), y)))
                  $ shapes i2
                  )
  }

placeImage :: Image -> Float -> Float -> Image -> Image
placeImage i1 x y i2 = Image
  { width  = width i2 -- Assuming that i2 is base (just like in htdp/image)
  , height = height i2
  , shapes =
    shapes i2
      ++ (map (\(p, (ox, oy)) -> (p, (ox + newX, oy + newY))) $ shapes i1)
  }
 where
  newX = convert 0 (negate $ (width i2) / 2) (width i2) ((width i2) / 2) x
  newY = convert 0 ((height i2) / 2) (height i2) (negate $ (height i2) / 2) y

overlay :: Image -> Image -> Image
overlay i1 i2 = placeImage i1 ((width i2) / 2) ((height i2) / 2) i2

underlay :: Image -> Image -> Image
underlay = flip overlay
