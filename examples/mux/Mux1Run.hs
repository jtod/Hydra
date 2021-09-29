-- Mux1Run: simulation driver for mux1 circuit
-- This file is part of Hydra.  John O'Donnell, 2021

module Main where
import HDL.Hydra.Core.Lib
import Mux1

test_data_1 :: [String]
test_data_1 =
-----------------------------------------
--   c  x  y       expected result
-----------------------------------------
  [ "0  0  0"  --  0  (c=0 so output=x)
  , "0  0  1"  --  0  (c=0 so output=x)
  , "0  1  0"  --  1  (c=0 so output=x)
  , "0  1  1"  --  1  (c=0 so output=x)
  , "1  0  0"  --  0  (c=1 so output=y)
  , "1  0  1"  --  1  (c=1 so output=y)
  , "1  1  0"  --  0  (c=1 so output=y)
  , "1  1  1"  --  1  (c=1 so output=y)
  ]

main :: IO ()
main = driver $ do

-- Input data
  useData test_data_1

-- Input ports  
  in_c <- inPortBit "c"
  in_x <- inPortBit "x"
  in_y <- inPortBit "y"

-- Input signals
  let c = inbsig in_c
  let x = inbsig in_x
  let y = inbsig in_y

-- Circuit
  let z = mymux1 c x y

-- Format the results  
  format
    [string "  c=", bit c,
     string "  x=", bit x,
     string "  y=", bit y,
     string "    output z=", bit z
    ]

-- Run the circuit on the test data
  runSimulation


  {-
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
-}
