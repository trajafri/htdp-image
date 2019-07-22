{-# LANGUAGE MultiWayIf, RecordWildCards #-}

module Graphics.Combinator
  ( Alignment
  , high
  , low
  , mid
  , overlay
  , overlayAlign
  , overlayOffset
  , overlayAlignOffset
  , overlayXY
  , underlay
  , underlayAlign
  , underlayOffset
  , underlayXY
  , underlayAlignOffset
  , beside
  , besides
  , besideAlign
  , besidesAlign
  , above
  , aboves
  , aboveAlign
  , abovesAlign
  , placeImage
  , placeImages
  , placeImageAlign
  , placeImagesAlign
  )
where

import           Graphics.Data.Image
import           Graphics.Util.Arithmetic
import           Prelude                 hiding ( Left
                                                , Right
                                                )

-- | Alignment position
data Alignment = Low | Mid | High

-- | Position to align at.
--   On x-axis, low means left, high means right.
--   On y-axis, low means bottom, high means top.
low, mid, high :: Alignment
low = Low
mid = Mid
high = High

-- Function to determine the alignment shift in above/beside
imageOffset :: (Image -> Float) -> Alignment -> Image -> Image -> Float
imageOffset dim al i1 i2 = case al of
  Low  -> lowAlign
  Mid  -> 0
  High -> negate lowAlign
  where lowAlign = (dim i2 - dim i1) / 2

-- | Constructs an image by placing @i1@ on top of @i2@, aligned along
--   the center.
above
  :: Image -- ^ @i1@
  -> Image -- ^ @i2@
  -> Image
above = aboveAlign mid

-- | Constructs an image by placing all images in a vertical row, aligned
--   along the center such that the first image in @is@ is at the top.
aboves
  :: [Image] -- ^ @is@
  -> Image
aboves = foldr1 above

-- | Constructs an image by placing @i1@ on top of @i2@, aligned as
--   specified by @al@.
aboveAlign
  :: Alignment -- ^ @al@
  -> Image     -- ^ @i1@
  -> Image     -- ^ @i2@
  -> Image
aboveAlign a i1 i2 = placeImage i2
                                (offset + width i1 / 2)
                                (height i1 + height i2 / 2)
                                i1
  where offset = imageOffset width a i1 i2

-- | Constructs an image by placing all images in a veritcal row, aligned
--   as specified by @al@ such that the first image in @is@ is at the top.
abovesAlign
  :: Alignment -- ^ @al@
  -> [Image]   -- ^ @is@
  -> Image
abovesAlign a = foldr1 $ aboveAlign a

-- | Constructs an image by placing @i1@ on the left of @i2@, aligned along
--   the center.
beside
  :: Image -- ^ @i1@
  -> Image -- ^ @i2@
  -> Image
beside = besideAlign mid

-- | Constructs an image by placing all images in a horizontal row, aligned
--   along the center such that the first image in @is@ is on the left.
besides
  :: [Image] -- ^ @is@
  -> Image
besides = foldr1 beside

-- | Constructs an image by placing @i1@ on the left of @i2@, aligned as
--   specified by @al@.
besideAlign
  :: Alignment -- ^ @al@
  -> Image     -- ^ @i1@
  -> Image     -- ^ @i2@
  -> Image
besideAlign a i1 i2 = placeImage i2
                                 (width i1 + width i2 / 2)
                                 (negate offset + height i1 / 2)
                                 i1
  where offset = imageOffset height a i1 i2

-- | Constructs an image by placing all images in a horizontal row, aligned
--   as specified by @al@ such that the first image in @is@ is on the left.
besidesAlign
  :: Alignment -- ^ @al@
  -> [Image]   -- ^ @is@
  -> Image
besidesAlign a = foldr1 $ besideAlign a

-- | Places @i1@ on top of @i2@ with @i1@'s center at position @(x, y)@.
--   Unlike 2htdp/image's place-image, placeImage increases the binding box
--   so that both images fit in it, instead of cropping parts of @i1@ that
--   lay outside of @i2@'s bounds.
placeImage
  :: Image -- ^ @i1@
  -> Float -- ^ @x@
  -> Float -- ^ @y@
  -> Image -- ^ @i2@
  -> Image
placeImage i1 x y = placeImageAlign i1 x y mid mid

-- | Places each @i@ in @is@ onto @i2@ using `placeImage`, using the coordinates
--   @(x, y)@ in @ps@.
placeImages
  :: [Image]          -- ^ @is@
  -> [(Float, Float)] -- ^ @ps@
  -> Image            -- ^ @i2@
  -> Image
placeImages is ps base =
  foldr (\(i1, (x, y)) i2 -> placeImage i1 x y i2) base $ zip is ps

-- | Like `placeImage`, but anchors @i1@ on @i2@ by the alignment specified
--   by @xAl@ and @yAl@.
placeImageAlign
  :: Image     -- ^ @i1@
  -> Float     -- ^ @x@
  -> Float     -- ^ @y@
  -> Alignment -- ^ @xAl@
  -> Alignment -- ^ @yAL@
  -> Image     -- ^ @i2@
  -> Image
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
  -- if i1 covers i2, don't move i1 at all, but shift i2
  -- if i1 is within i2, don't move i2 at all, but shift i1
  -- else, move both
  newShapes
    | newW == width i1 && newH == height i1
    = [ (p, (ox - newX, oy - newY)) | (p, (ox, oy)) <- shapes i2 ] ++ shapes i1
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


-- | Like `placeImages`, but anchors @is@ on @i2@ by the alignment specified
--   by @xAl@ and @yAl@.
placeImagesAlign
  :: [Image]          -- ^ @is@
  -> [(Float, Float)] -- ^ @ps@
  -> Alignment        -- ^ @xAl@
  -> Alignment        -- ^ @yAl@
  -> Image            -- ^ @i2@
  -> Image
placeImagesAlign is ps xAl yAl b =
  foldr (\(i1, (x, y)) i2 -> placeImageAlign i1 x y xAl yAl i2) b $ zip is ps

-- | Places @i1@ on the center of @i2@.
overlay
  :: Image -- ^ @i1@
  -> Image -- ^ @i2@
  -> Image
overlay i1 i2 = placeImage i1 (width i2 / 2) (height i2 / 2) i2

-- | Places @i1@ on top of @i2@ and uses @xAl@ and @yAl@ for alignment.
overlayAlign
  :: Alignment -- ^ @xAl@
  -> Alignment -- ^ @yal@
  -> Image     -- ^ @i1@
  -> Image     -- ^ @i2@
  -> Image
overlayAlign xAl yAl i1 = overlayAlignOffset xAl yAl i1 0 0

-- | Places @i1@ on top of @i2@ and moves @i2@ by @x@ pixels to the right,
--   and @y@ pixels down.
overlayOffset
  :: Image -- ^ @i1@
  -> Float -- ^ @x@
  -> Float -- ^ @y@
  -> Image -- ^ @i2@
  -> Image
overlayOffset = overlayAlignOffset mid mid

-- | Places @i1@ on top of @i2@ by lining them on their top left corners,
--   then @i2@ is shifted to the right by @x@ pixels and down by @y@ pixels.
overlayXY
  :: Image -- ^ @i1@
  -> Float -- ^ @x@
  -> Float -- ^ @y@
  -> Image -- ^ @i2@
  -> Image
overlayXY i1 x y i2 = placeImage i1 (width i1 / 2 - x) (height i1 / 2 - y) i2

-- | Combination of `overlayAlign` and `overlayOffset`.
overlayAlignOffset
  :: Alignment -> Alignment -> Image -> Float -> Float -> Image -> Image
overlayAlignOffset xAl yAl i1 x y i2 = placeImageAlign i1
                                                       shiftedX
                                                       shiftedY
                                                       mid
                                                       mid
                                                       i2
 where
  newX  = (width i2 / 2 - x)
  newY  = (height i2 / 2 - y)
  wDiff = (width i2 - width i1) / 2
  hDiff = (height i2 - height i1) / 2
  shiftedX =
    newX
      + (case xAl of
          Low  -> (-wDiff)
          Mid  -> 0
          High -> wDiff
        )
  shiftedY =
    newY
      + (case yAl of
          Low  -> hDiff
          Mid  -> 0
          High -> (-hDiff)
        )

-- | Same of `overlay`, but with image arguments flipped.
underlay :: Image -> Image -> Image
underlay = flip overlay

-- | Same of `overlayAlign`, but with image arguments flipped.
underlayAlign :: Alignment -> Alignment -> Image -> Image -> Image
underlayAlign xAl yAl i1 i2 = overlayAlign xAl yAl i2 i1

-- | Same of `overlayOffset`, but with image arguments flipped.
underlayOffset :: Image -> Float -> Float -> Image -> Image
underlayOffset i1 x y i2 = overlayOffset i2 (negate x) (negate y) i1

-- | Same of `overlayAlignOffset`, but with image arguments flipped.
underlayAlignOffset
  :: Alignment -> Alignment -> Image -> Float -> Float -> Image -> Image
underlayAlignOffset xAl yAl i1 x y i2 =
  overlayAlignOffset xAl yAl i2 (negate x) (negate y) i1

-- | Same of `overlayXY`, but with image arguments flipped.
underlayXY :: Image -> Float -> Float -> Image -> Image
underlayXY i1 x y i2 = overlayXY i2 (negate x) (negate y) i1
