{-# LANGUAGE MultiWayIf #-}

-- Sorted as they are in 2htdp/image
-- | Image constructors
module Graphics.Shape
  ( Mode
  , solid
  , outline
  , circle
  , ellipse
  , line
  , addLine
  , emptyImage
  , triangle
  , rightTriangle
  , isoscelesTriangle
  -- *** The following image is useful for the following family of functions.
  -- |    [triangleFamilyPic](https://docs.racket-lang.org/teachpack/triangle-xxx.png)
  , triangleSSS
  , triangleASS
  , triangleSAS
  , triangleSSA
  , triangleAAS
  , triangleASA
  , triangleSAA
  , square
  , rectangle
  , rhombus
  , star
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

-- | Drawing mode.
data Mode = Solid | Outline deriving Eq

-- | Type of drawing mode.
solid, outline :: Mode
solid = Solid
outline = Outline

-- Initial point for all images
origin :: G.Point
origin = (0, 0)

-- | Adds a line to the given image @i@ of color @c@, starting from point @(x1, y1)@
--   and going to point @(x2, y2)@. If the line crosses the given image's binding box,
--   then new image dimesions are changed to accommodate the line.
addLine
  :: Image -- ^ @i@
  -> Float -- ^ @x1@
  -> Float -- ^ @y1@
  -> Float -- ^ @x2@
  -> Float -- ^ @y2@
  -> Color
  -> Image
addLine i x1 y1 x2 y2 c =
  placeImage (line (x1 - x2) (y1 - y2) c) ((x1 + x2) / 2) ((y1 + y2) / 2) i

-- | Constructs a circle of radius @r@, drawing mode @m@ and color @c@.
circle
  :: Float -- ^ @r@
  -> Mode  -- ^ @m@
  -> Color -- ^ @c@
  -> Image
circle r mode c = Image { width  = r * 2
                        , height = r * 2
                        , shapes = [(G.color c $ circleKind r, origin)]
                        }
 where
  circleKind = case mode of
    Solid   -> G.circleSolid
    Outline -> G.circle

-- | Constructs an ellipse of width @w@, height @h@, mode @m@, and color @c@.
ellipse
  :: Float -- ^ @w@
  -> Float -- ^ @h@
  -> Mode  -- ^ @m@
  -> Color -- ^ @c@
  -> Image
ellipse w h m c = Image { width  = w
                        , height = h
                        , shapes = [(circleToEllipse, origin)]
                        }
 where -- This took me longer than it should have
  circleToEllipse = G.scale (w / (2 * radius)) (h / (2 * radius)) circPic
  circPic         = fst . head . shapes $ circle radius m c
  radius          = (w + h) / 4

-- | Constructs an image of width and height @0@.
emptyImage :: Image
emptyImage = Image 0 0 []

-- | Constructs a triangle of two equal-length sides, of length @l@, where the
--   angle between those sides is @a@, mode is @m@ and color is @c@. If the angle
--   is less than @180@, then the triangle will point up, else it will point down.
isoscelesTriangle
  :: Float -- ^ @l@
  -> Float -- ^ @a@
  -> Mode  -- ^ @m@
  -> Color -- ^ @c@
  -> Image
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

-- | Constructs an image of a line segment of color @c@ that connects the points
--   @(0,0)@ to @(x1, y1)@.
line
  :: Float -- ^ @x1@
  -> Float -- ^ @y1@
  -> Color -- ^ @c@
  -> Image
line x y c = Image { width  = abs x
                   , height = abs y
                   , shapes = [(G.color c $ G.Line lineShape, origin)]
                   }
  -- We want the line centered on the origin.
  -- For that, we will need to move the given points
  -- (I had to take a day off to get this)
  where lineShape = [(x / 2, negate y / 2), (negate x / 2, y / 2)]

-- | Constructs a rectangle of width @w@, height @h@, mode @m@, and color @c@.
rectangle
  :: Float -- ^ @w@
  -> Float -- ^ @h@
  -> Mode  -- ^ @m@
  -> Color -- ^ @c@
  -> Image
rectangle w h mode c = Image { width  = w
                             , height = h
                             , shapes = [(G.color c $ rectShape w h, origin)]
                             }
 where
  rectShape = case mode of
    Solid   -> G.rectangleSolid
    Outline -> G.rectangleWire

-- | Constructs a four sided polygon with all equal sides of length @l@, where the
--   top and bottom pair of angles is @a@, and the left and right are @180 - a@.
--   As usual, mode is @m@ and color is @c@.
rhombus
  :: Float -- ^ @l@
  -> Float -- ^ @a@
  -> Mode  -- ^ @m@
  -> Color -- ^ @c@
  -> Image
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

-- | Constructs a right triangle with base length @b@, perpendicular length
--   @p@, mode @m@, and color @c@.
rightTriangle
  :: Float -- ^ @b@
  -> Float -- ^ @p@
  -> Mode  -- ^ @m@
  -> Color -- ^ @c@
  -> Image
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

-- | Constructs a square of side @s@, mode @m@, and color @c@.
square
  :: Float -- ^ @s@
  -> Mode  -- ^ @m@
  -> Color -- ^ @c@
  -> Image
square w = rectangle w w

-- | Constructs a star with five points of mode @m@ and color @c@. The argument
--   @l@ determines the side length of the internal pentagon.
--   Currently, a solid star is glitchy since it is a non-convex polygon
--   and openGL (the underlying graphics library) doesn't draw them correctly.
--   This will be corrected in future versions.
star
  :: Float -- ^ @l@
  -> Mode  -- ^ @m@
  -> Color -- ^ @c@
  -> Image
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

-- | Constructs an upward-pointing equilateral triangle with length @l@,
--   mode @m@, and color @c@.
triangle
  :: Float -- ^ @l@
  -> Mode  -- ^ @m@
  -> Color -- ^ @c@
  -> Image
triangle sideLength = isoscelesTriangle sideLength 60

-- | Constructs a triangle of mode @m@, color @color@, angle @A@, angle @B@, and
--   length @c@. The variables refer to the diagram above.
--   If it's not possible to construct the triangle with the given arguments,
--   an empty image is returned.
triangleAAS
  :: Float -- ^ @A@
  -> Float -- ^ @B@
  -> Float -- ^ @c@
  -> Mode  -- ^ @m@
  -> Color -- ^ @color@
  -> Image
triangleAAS degr degl t = triangleSSS
  (t * (sine . Degrees $ degr) / (sine . Degrees $ 180 - (degl + degr)))
  (t * (sine . Degrees $ degl) / (sine . Degrees $ 180 - (degl + degr)))
  t

-- | Constructs a triangle of mode @m@, color @color@, angle @A@, angle @C@, and
--   length @b@. The variables refer to the diagram above.
--   If it's not possible to construct the triangle with the given arguments,
--   an empty image is returned.
triangleASA
  :: Float -- ^ @A@
  -> Float -- ^ @C@
  -> Float -- ^ @b@
  -> Mode  -- ^ @m@
  -> Color -- ^ @color@
  -> Image
triangleASA degl l degt = triangleSSS
  (l * (sine . Degrees $ degl) / (sine . Degrees $ 180 - (degt + degl)))
  l
  (l * (sine . Degrees $ degt) / (sine . Degrees $ 180 - (degt + degl)))

-- | Constructs a triangle of mode @m@, color @color@, angle @B@, angle @C@, and
--   length @a@. The variables refer to the diagram above.
--   If it's not possible to construct the triangle with the given arguments,
--   an empty image is returned.
triangleSAA
  :: Float -- ^ @B@
  -> Float -- ^ @C@
  -> Float -- ^ @a@
  -> Mode  -- ^ @m@
  -> Color -- ^ @color@
  -> Image
triangleSAA r degr degt = triangleSSS
  r
  (r * (sine . Degrees $ degr) / (sine . Degrees $ 180 - (degt + degr)))
  (r * (sine . Degrees $ degt) / (sine . Degrees $ 180 - (degt + degr)))

-- | Constructs a triangle of mode @m@, color @color@, angle @A@, and lengths
--   @b@ and @c@. The variables refer to the diagram above.
--   If it's not possible to construct the triangle with the given arguments,
--   an empty image is returned.
triangleASS
  :: Float -- ^ @A@
  -> Float -- ^ @b@
  -> Float -- ^ @c@
  -> Mode  -- ^ @mode@
  -> Color -- ^ @color@
  -> Image
triangleASS deg l t = triangleSSS
  (((l ** 2) + (t ** 2) - 2 * t * l * (cosine . Degrees $ deg)) ** (1 / 2))
  l
  t

-- | Constructs a triangle of mode @m@, color @color@, angle @B@, and lengths
--   @a@ and @c@. The variables refer to the diagram above.
--   If it's not possible to construct the triangle with the given arguments,
--   an empty image is returned.
triangleSAS
  :: Float -- ^ @B@
  -> Float -- ^ @a@
  -> Float -- ^ @c@
  -> Mode  -- ^ @mode@
  -> Color -- ^ @color@
  -> Image
triangleSAS r deg t = triangleSSS
  r
  (((r ** 2) + (t ** 2) - 2 * t * r * (cosine . Degrees $ deg)) ** (1 / 2))
  t

-- | Constructs a triangle of mode @m@, color @color@, angle @C@, and lengths
--   @a@ and @c@. The variables refer to the diagram above.
--   If it's not possible to construct the triangle with the given arguments,
--   an empty image is returned.
triangleSSA
  :: Float -- ^ @C@
  -> Float -- ^ @a@
  -> Float -- ^ @c@
  -> Mode  -- ^ @mode@
  -> Color -- ^ @color@
  -> Image
triangleSSA r l deg = triangleSSS
  r
  l
  (((r ** 2) + (l ** 2) - 2 * l * r * (cosine . Degrees $ deg)) ** (1 / 2))

-- | Constructs a triangle of side @a@, @b@, and @c@. The variables refer to the
--   diagram above.
--   If it's not possible to construct the triangle with the given arguments,
--   an empty image is returned.
triangleSSS
  :: Float -- ^ @a@
  -> Float -- ^ @b@
  -> Float -- ^ @c@
  -> Mode  -- ^ @m@
  -> Color -- ^ @color@
  -> Image
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
