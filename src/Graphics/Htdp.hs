{-| htdp-image is a simple graphics library written on top of [Gloss](https://hackage.haskell.org/package/gloss-1.13.0.1/docs/Graphics-Gloss.html)
    that allows users to create complex images by combining smaller images.
    
    For example, four iterations of the sierpinski triangle can be drawn as:
    
    @
    import Graphics.Htdp
    main = drawImage $ sier . sier . sier . sier $ triangle 20 solid red
     where
     sier :: Image -> Image
     sier t = above t (beside t t)
    @
  
    Once the image is drawn, you can use Gloss key bindings to navigate around.
-}

module Graphics.Htdp
  ( module Graphics.Gloss.Data.Color
  , module Graphics.Shape
  , module Graphics.Combinator
  , drawImage
  , Image
  , width
  , height
  )
where

import qualified Graphics.Gloss                as G
import           Graphics.Gloss.Data.Color
import           Graphics.Combinator
import           Graphics.Data.Image
import           Graphics.Shape

-- | Function to draw an image in a new window with same dimensions as the given image.
drawImage :: Image -> IO ()
drawImage i =
  G.display
      (G.InWindow "htdp-image"
                  (succ . round . width $ i, succ . round . height $ i)
                  (0                       , 0)
      )
      G.white
    $ G.Pictures (map (\(p, (x, y)) -> G.translate x y p) (shapes i))
