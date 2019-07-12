module Graphics.Shape
  ( Mode(..)
  , circle
  )
where

import           Graphics.Data.Image
import qualified Graphics.Gloss                as G
import           Graphics.Gloss.Data.Color

data Mode = Solid | Outline deriving (Show, Eq)

origin :: G.Point
origin = (0, 0)

circle :: Float -> Mode -> Color -> Image
circle r Outline c = Image { width  = r * 2
                           , height = r * 2
                           , shapes = [(G.color c $ G.circle r, origin)]
                           }
circle r Solid c = Image { width  = r * 2
                         , height = r * 2
                         , shapes = [(G.color c $ G.circleSolid r, origin)]
                         }


