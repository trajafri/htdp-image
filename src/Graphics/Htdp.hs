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

drawImage :: Image -> IO ()
drawImage i =
  G.display
      (G.InWindow "htdp-image" (round . width $ i, round . height $ i) (0, 0))
      G.white
    $ G.Pictures (map (\(p, (x, y)) -> G.translate x y p) (shapes i))
