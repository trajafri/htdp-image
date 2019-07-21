module Graphics.Util.Arithmetic
  ( computeHypotenuse
  , computeRightSide
  , convert
  , distance
  , heron
  )
where

computeHypotenuse :: Float -> Float -> Float
computeHypotenuse a b = (a ** 2 + b ** 2) ** (1 / 2)

computeRightSide :: Float -> Float -> Float
computeRightSide h a = (h ** 2 - a ** 2) ** (1 / 2)

convert :: Float -> Float -> Float -> Float -> Float -> Float
convert a1 a2 b1 b2 c1 = (((c1 - a1) * (b2 - a2)) / (b1 - a1)) + a2

{-   y_a1  = 0x + ba                    y_a2 = 0x + bb
       a1  =      ba                      a2 =      bb
------------------------------------------------------
     y_b1  = m  + ba                    y_b2 = M  + bb
   b1 - ba = m                       b2 - bb = M
   b1 - a1 = m                       b2 - a2 = M
------------------------------------------------------
      y_c1 = mx + ba                    y_c2 = Mx + bb
 y_c1 - ba = mx                    y_c2 - bb = Mx
   c1 - a1 = (b1 - a1)x              c2 - a2 = (b2 - a2)x
                                     c2 - a2
                                     ------- = x
                                     b2 - a2

            c1 - a1 = (b1 - a1)(c2 - a2)
                      -----------------
                          (b2 - a2)

          (c1 - a1)(b2 - a2)
          ------------------ = c2 - a2
               (b1 - a1)


          c2 = (c1 - a1)(b2 - a2) + a2
               ------------------
                    (b1 - a1)

Thanks RRose
-}

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = ((y2 - y1) ** 2 + (x2 - x1) ** 2) ** (1 / 2)

heron :: Float -> Float -> Float -> Float
heron a b c = (p * (p - a) * (p - b) * (p - c)) ** (1 / 2)
  where p = (a + b + c) / 2
