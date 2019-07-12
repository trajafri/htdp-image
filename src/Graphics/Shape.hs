module Graphics.Shape
  ( Mode(..)
  , circle
  , triangle
  )
where

import           Graphics.Data.Image
import qualified Graphics.Gloss                as G
import           Graphics.Gloss.Data.Color

data Mode = Solid | Outline deriving (Show, Eq)

origin :: G.Point
origin = (0, 0)

circle :: Float -> Mode -> Color -> Image
circle r mode c = Image { width  = r * 2
                        , height = r * 2
                        , shapes = [(G.color c $ circleKind r, origin)]
                        }
 where
  circleKind = case mode of
    Solid   -> G.circleSolid
    Outline -> G.circle

triangle :: Float -> Mode -> Color -> Image
triangle sideLength mode c = Image
  { width  = tW
  , height = tH
  , shapes = [(G.color c $ triangleShape, origin)]
  }
 where
  tW = sideLength
  tH = ((tW ** 2) - ((sideLength / 2) ** 2)) ** (1 / 2)
  tShape = --left, right, top
    [ ((negate $ tW / 2), negate $ tH / 2)
    , (tW / 2           , negate $ tH / 2)
    , (0                , tH / 2)
    ]
  triangleShape = case mode of
    Solid   -> G.polygon tShape
    Outline -> G.line ((0, tH / 2) : tShape)
