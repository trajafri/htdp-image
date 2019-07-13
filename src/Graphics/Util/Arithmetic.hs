module Graphics.Util.Arithmetic where

computeHypotenuse :: Float -> Float -> Float
computeHypotenuse a b = (a ** 2 + b ** 2) ** (1 / 2)

computeRightSide :: Float -> Float -> Float
computeRightSide h a = (h ** 2 - a ** 2) ** (1 / 2)

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = ((y2 - y1) ** 2 + (x2 - x1) ** 2) ** (1 / 2)
