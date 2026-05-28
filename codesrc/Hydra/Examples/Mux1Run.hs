-- Mux1Run: test the mux1 circuit
module Main where
import Hydra.Core
import Hydra.Examples.Mux1


testdata :: [String]
testdata =
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
  useData testdata

-- Input signals  
  c <- inputBit "c"
  x <- inputBit "x"
  y <- inputBit "y"

-- The circuit
  let z = mux1 c x y

-- Output signals
  outputBit "z" z

-- Run
  runCircuit
