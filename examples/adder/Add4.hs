module Add4 where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

-- A 4-bit binary adder.  The fullAdd circuit is imported from the
-- Combinational library.  There is a better (but more advanced) way
-- to specify this circuit; see User Manual > Combinators > Scan.

add4 :: Bit a => a -> [a] -> [a] -> (a,[a])
add4 cin [x0, x1, x2, x3] [y0, y1, y2, y3]
  = (c0, [s0,s1,s2,s3])
  where
    (c0,s0) = fullAdd (x0,y0) c1
    (c1,s1) = fullAdd (x1,y1) c2
    (c2,s2) = fullAdd (x2,y2) c3
    (c3,s3) = fullAdd (x3,y3) cin
