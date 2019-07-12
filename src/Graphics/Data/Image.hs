module Graphics.Data.Image
  ( Image(..)
  , emptyScene
  )
where

import           Graphics.Gloss

data Image = Image {width :: Float , height :: Float , shapes :: [(Picture, Point)] }

emptyScene :: Float -> Float -> Image
emptyScene w h = Image { width = w, height = h, shapes = [] }
