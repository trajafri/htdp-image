{-# LANGUAGE RecordWildCards #-}

-- | Image related utilities.
module Graphics.Htdp.Data.Image
  ( Image(..)
  , rotate
  )
where

import           Data.Angle
import qualified Graphics.Gloss                as G

-- | A 2D Image.
data Image = Image {width :: Float ,                 -- ^ Width of image
                    height :: Float ,                -- ^ Height of image
                    shapes :: [(G.Picture, G.Point)] -- ^ Collection of all Gloss Pictures that create the Image.
                   } deriving Eq

-- | Rotates @i@ by @deg@ degrees in a counter-clockwise direction.
--   Unlike 2htdp/image's rotate function, this function is not smart enough
--   to reduce the rotated image's binding box to fit the actual image.
--   Instead, it just creates a new binding box so that @i@'s binding box
--   fits in it.
rotate
  :: Float -- ^ @deg@
  -> Image -- ^ @i@
  -> Image
rotate deg Image {..} = Image newW newH
  $ map (\(p, c) -> (G.rotate (negate deg) p, rotateC c)) shapes
 where
  newW =
    width
      * (abs . sine . Degrees $ 90 - deg)
      + height
      * (abs . sine . Degrees $ deg)
  newH =
    width
      * (abs . sine . Degrees $ deg)
      + height
      * (abs . sine . Degrees $ 90 - deg)
  rotateC :: (Float, Float) -> (Float, Float)
  rotateC (x, y) =
    ( x * (cosine . Degrees $ deg) + y * (negate . sine . Degrees $ deg)
    , x * (sine . Degrees $ deg) + y * (cosine . Degrees $ deg)
    )

