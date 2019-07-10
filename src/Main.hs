module Main
  ( Image
  , Point
  , Mode
  , module Graphics.Gloss.Data.Color
  , above
  , beside
  , placeImage
  , drawImage
  , circle
  , emptyScene
  )
where

import qualified Graphics.Gloss                as G
import           Graphics.Gloss.Data.Color

type Point = G.Point

data Image = Image {width :: Float , height :: Float , shapes :: [(G.Picture, Point)] }

data Mode = Solid | Outline deriving (Show, Eq)

origin :: Point
origin = (0, 0)

emptyScene :: Float -> Float -> Image
emptyScene w h = Image { width = w, height = h, shapes = [] }

circle :: Float -> Mode -> Color -> Image
circle r Outline c = Image { width  = r * 2
                           , height = r * 2
                           , shapes = [(G.color c $ G.circle r, origin)]
                           }
circle r Solid c = Image { width  = r * 2
                         , height = r * 2
                         , shapes = [(G.color c $ G.circleSolid r, origin)]
                         }

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
  , shapes = shapes i2
               ++ (map (\(p, (ox, oy)) -> (p, (ox + x, oy + y))) $ shapes i1)
  }

drawImage :: Image -> IO ()
drawImage i =
  G.display
      (G.InWindow "htdp-image" (round . width $ i, round . height $ i) (0, 0))
      G.white
    $ G.Pictures (map (\(p, (x, y)) -> G.translate x y p) (shapes i))
