module Graphics.Shape
  ( Mode(..)
  , circle
  , ellipse
  , emptyImage
  , line
  , rectangle
  , rhombus
  , square
  , triangle
  )
where

import           Data.Angle
import           Graphics.Data.Image
import qualified Graphics.Gloss                as G
import           Graphics.Gloss.Data.Color
import           Graphics.Util.Arithmetic

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

ellipse :: Float -> Float -> Mode -> Color -> Image
ellipse w h m c = Image { width  = w * 2
                        , height = h * 2
                        , shapes = [(circleToEllipse, origin)]
                        }
 where -- This took me longer than it should have
  circleToEllipse = G.scale (w / radius) (h / radius) circPic
  circPic         = fst . head . shapes $ circle radius m c
  radius          = (w + h) / 2

emptyImage :: Image
emptyImage = Image 0 0 []

line :: Float -> Float -> Color -> Image
line x y c = Image { width  = abs x
                   , height = abs y
                   , shapes = [(G.color c $ G.Line lineShape, origin)]
                   }
  -- We want the line centered on the origin.
  -- For that, we will need to move the given points
  -- (I had to take a day off to get this)
  where lineShape = [(x / 2, negate y / 2), (negate x / 2, y / 2)]

rectangle :: Float -> Float -> Mode -> Color -> Image
rectangle w h mode c = Image { width  = w
                             , height = h
                             , shapes = [(G.color c $ rectShape w h, origin)]
                             }
 where
  rectShape = case mode of
    Solid   -> G.rectangleSolid
    Outline -> G.rectangleWire

rhombus :: Float -> Float -> Mode -> Color -> Image
rhombus sideLength angle m c = Image { width  = base
                                     , height = opp
                                     , shapes = [(G.color c $ rShape, origin)]
                                     }
 where
  -- It's the law of Cosine bb
  base = (2 * (sideLength ** 2) * (1 - (cosine . Degrees $ angle))) ** (1 / 2)
  opp  = 2 * computeRightSide sideLength (base / 2)
  rhombusShape =
    [(negate base / 2, 0), (0, opp / 2), (base / 2, 0), (0, negate opp / 2)]
  rShape = case m of
    Solid   -> G.polygon rhombusShape
    Outline -> G.line ((0, negate opp / 2) : rhombusShape)

square :: Float -> Mode -> Color -> Image
square w = rectangle w w

triangle :: Float -> Mode -> Color -> Image
triangle sideLength mode c = Image
  { width  = tW
  , height = tH
  , shapes = [(G.color c $ triangleShape, origin)]
  }
 where
  tW = sideLength
  tH = computeRightSide tW (sideLength / 2)
  tShape = --left, right, top
    [ ((negate $ tW / 2), negate $ tH / 2)
    , (tW / 2           , negate $ tH / 2)
    , (0                , tH / 2)
    ]
  triangleShape = case mode of
    Solid   -> G.polygon tShape
    Outline -> G.line ((0, tH / 2) : tShape)
