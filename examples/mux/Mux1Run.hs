-- Mux1Run: simulation driver for mux1 circuit
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module Main where
import HDL.Hydra.Core.Lib
import Mux1

main :: IO ()
main = muxDriver testData1

testData1 :: [String]
testData1 =
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


muxDriver :: [String] -> IO ()
muxDriver x = driver $ do

-- Input data
  useData x

-- Inputs
  c <- inputBit "c"
  x <- inputBit "x"
  y <- inputBit "y"

-- Circuit
  let z = mymux1 c x y
-- Outputs
  outputBit "z" z

-- Run
  runSimulation
