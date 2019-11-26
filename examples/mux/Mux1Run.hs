module Main where
import HDL.Hydra.Core.Lib
import Mux1

main :: IO ()
main = mux1Run testdata

testdata :: [[Int]]
testdata =
-----------------------------------------
--   c  x  y       expected result
-----------------------------------------
  [ [0, 0, 0]  --  0  (c=0 so output=x)
  , [0, 0, 1]  --  0  (c=0 so output=x)
  , [0, 1, 0]  --  1  (c=0 so output=x)
  , [0, 1, 1]  --  1  (c=0 so output=x)
  , [1, 0, 0]  --  0  (c=1 so output=y)
  , [1, 0, 1]  --  1  (c=1 so output=y)
  , [1, 1, 0]  --  0  (c=1 so output=y)
  , [1, 1, 1]  --  1  (c=1 so output=y)
  ]

mux1Run input = runAllInput input output
  where
-- Extract input signals  
    c = getbit input 0
    x = getbit input 1
    y = getbit input 2
-- The circuit to be simulated
    z = mymux1 c x y
-- Format the output
    output =
      [string "  c=", bit c,
       string "  x=", bit x,
       string "  y=", bit y,
       string "    output z=", bit z
      ]
