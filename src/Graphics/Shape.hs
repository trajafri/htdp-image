{-# LANGUAGE MultiWayIf #-}

module Graphics.Shape
  ( Mode
  , addLine
  , circle
  , ellipse
  , emptyImage
  , isoscelesTriangle
  , line
  , outline
  , rectangle
  , rhombus
  , rightTriangle
  , solid
  , square
  , star
  , triangle
  , triangleAAS
  , triangleASA
  , triangleASS
  , triangleSAA
  , triangleSAS
  , triangleSSA
  , triangleSSS
  )
where

import           Data.Angle
import           Data.Fixed
import           Data.List
import           Graphics.Combinator
import           Graphics.Data.Image
import qualified Graphics.Gloss                as G
import           Graphics.Gloss.Data.Color
import           Graphics.Util.Arithmetic

data Mode = Solid | Outline deriving Eq

solid, outline :: Mode
solid = Solid
outline = Outline

origin :: G.Point
origin = (0, 0)

addLine :: Image -> Float -> Float -> Float -> Float -> Color -> Image
addLine i x1 y1 x2 y2 c =
  placeImage (line (x1 - x2) (y1 - y2) c) ((x1 + x2) / 2) ((y1 + y2) / 2) i

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
ellipse w h m c = Image { width  = w
                        , height = h
                        , shapes = [(circleToEllipse, origin)]
                        }
 where -- This took me longer than it should have
  circleToEllipse = G.scale (w / (2 * radius)) (h / (2 * radius)) circPic
  circPic         = fst . head . shapes $ circle radius m c
  radius          = (w + h) / 4

emptyImage :: Image
emptyImage = Image 0 0 []

isoscelesTriangle :: Float -> Float -> Mode -> Color -> Image
isoscelesTriangle sl deg m c = Image
  { width  = newW
  , height = newH
  , shapes = [(G.color c triangleShape, origin)]
  }
 where
  newW   = (2 * (sl ** 2) * (1 - (cosine . Degrees $ deg))) ** (1 / 2)
  newH   = computeRightSide sl (newW / 2)
  topDir = if mod' deg 360 < 180 then 1 else -1
  tShape =
    [ (negate newW / 2, negate topDir * (newH / 2))
    , (0              , topDir * newH / 2)
    , (newW / 2       , negate topDir * (newH / 2))
    ]
  triangleShape = case m of
    Solid   -> G.polygon tShape
    Outline -> G.line ((newW / 2, negate topDir * (newH / 2)) : tShape)


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
                                     , shapes = [(G.color c rShape, origin)]
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

rightTriangle :: Float -> Float -> Mode -> Color -> Image
rightTriangle b p m c = Image { width  = b
                              , height = p
                              , shapes = [(G.color c triangleShape, origin)]
                              }
 where
  tShape =
    [(b / 2, negate p / 2), (negate b / 2, p / 2), (negate b / 2, negate p / 2)]
  triangleShape = case m of
    Solid   -> G.polygon tShape
    Outline -> G.line ((negate b / 2, negate p / 2) : tShape)

square :: Float -> Mode -> Color -> Image
square w = rectangle w w

star :: Float -> Mode -> Color -> Image
star side m c = Image { width  = w
                      , height = h
                      , shapes = [(G.color c sShape, origin)]
                      }
 where
    -- Pentagon is 108degs apart
  w          = (2 * triHyp) + side
  h          = (1.539 * side) + triPerp + bottomPerp
  triHyp     = (sine . Degrees $ 72) * (side / (sine . Degrees $ 36))
  triPerp    = computeRightSide triHyp (side / 2)
  bottomPerp = computeRightSide triHyp (bottom / 2)
  bottom     = (2 * (triHyp ** 2) * (1 - (cosine . Degrees $ 108))) ** (1 / 2)
  sShape     = case m of
    Solid ->
      G.polygon
        $ concat
        . transpose
        $ [[bLeftSt, bRightSt, rightSt, topSt, leftSt], pentPoints]
    Outline -> G.line (rightSt : starPoints) -- Some hack to fix solid
  starPoints = [bLeftSt, topSt, bRightSt, leftSt, rightSt]
  topSt      = (0, h / 2)
  leftSt     = (negate (w / 2), (h / 2) - triPerp)
  rightSt    = (negate . fst $ leftSt, snd leftSt)
  bLeftSt    = (negate (bottom / 2), negate (h / 2))
  bRightSt   = (negate . fst $ bLeftSt, snd bLeftSt)
  pentPoints = [bottomP, bRightP, rightP, leftP, bLeftP]
  bottomP    = (0, negate $ h / 2 - bottomPerp)
  leftP      = (negate $ side / 2, snd leftSt)
  rightP     = (negate . fst $ leftP, snd leftP)
  bLeftP     = (negate midPerp, negate $ h / 2 - (bottomPerp + midPerp))
  bRightP    = (negate . fst $ bLeftP, snd bLeftP)
  midPerp    = computeRightSide side $ (1.618 * side) / 2

triangle :: Float -> Mode -> Color -> Image
triangle sideLength = isoscelesTriangle sideLength 60

triangleAAS :: Float -> Float -> Float -> Mode -> Color -> Image
triangleAAS degr degl t = triangleSSS
  (t * (sine . Degrees $ degr) / (sine . Degrees $ 180 - (degl + degr)))
  (t * (sine . Degrees $ degl) / (sine . Degrees $ 180 - (degl + degr)))
  t

triangleASA :: Float -> Float -> Float -> Mode -> Color -> Image
triangleASA degl l degt = triangleSSS
  (l * (sine . Degrees $ degl) / (sine . Degrees $ 180 - (degt + degl)))
  l
  (l * (sine . Degrees $ degt) / (sine . Degrees $ 180 - (degt + degl)))

triangleSAA :: Float -> Float -> Float -> Mode -> Color -> Image
triangleSAA r degr degt = triangleSSS
  r
  (r * (sine . Degrees $ degr) / (sine . Degrees $ 180 - (degt + degr)))
  (r * (sine . Degrees $ degt) / (sine . Degrees $ 180 - (degt + degr)))

triangleASS :: Float -> Float -> Float -> Mode -> Color -> Image
triangleASS deg l t = triangleSSS
  (((l ** 2) + (t ** 2) - 2 * t * l * (cosine . Degrees $ deg)) ** (1 / 2))
  l
  t

triangleSAS :: Float -> Float -> Float -> Mode -> Color -> Image
triangleSAS r deg t = triangleSSS
  r
  (((r ** 2) + (t ** 2) - 2 * t * r * (cosine . Degrees $ deg)) ** (1 / 2))
  t

triangleSSA :: Float -> Float -> Float -> Mode -> Color -> Image
triangleSSA r l deg = triangleSSS
  r
  l
  (((r ** 2) + (l ** 2) - 2 * l * r * (cosine . Degrees $ deg)) ** (1 / 2))

-- If it's not possible to construct the requested triangel, an empty image is returned
triangleSSS :: Float -> Float -> Float -> Mode -> Color -> Image
triangleSSS r l t m c =
  if (round . distance (bottX, negate newH / 2) $ (-t / 2, newH / 2) :: Integer)
     == (round l)
     && (round . distance (bottX, negate newH / 2) $ (t / 2, newH / 2) :: Integer
        )
     == round r
  then
    Image { width  = newW
          , height = newH
          , shapes = [(G.color c triangleShape, origin)]
          }
  else
    emptyImage

 where
  angleL =
    arccosine $ (l ** 2 - r ** 2 - t ** 2) / (-2 * r * t) :: Degrees Float
  angleR =
    arccosine $ (r ** 2 - l ** 2 - t ** 2) / (-2 * l * t) :: Degrees Float
  newH = (if angleR < angleL then l else r) * (sine $ min angleR angleL)
  newW = if
    | angleR < angleL && angleL > Degrees 90
    -> l * (sine $ Degrees 90 - min angleR angleL)
    | angleL < angleR && angleR > Degrees 90
    -> r * (sine $ Degrees 90 - min angleR angleL)
    | otherwise
    -> t
  bottW = computeRightSide (max l r) newH
  bottX = if l > r then bottW - (t / 2) else negate $ bottW - (t / 2)
  converter =
    convert (min bottX $ -t / 2) (-newW / 2) (max bottX $ t / 2) (newW / 2)
  tShape =
    [ (converter $ negate t / 2, newH / 2)
    , (converter bottX         , negate newH / 2)
    , (converter $ t / 2       , newH / 2)
    ]
  triangleShape = case m of
    Solid   -> G.polygon tShape
    Outline -> G.line ((converter $ t / 2, newH / 2) : tShape)
