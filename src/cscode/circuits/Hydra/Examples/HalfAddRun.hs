module Hydra.Examples.HalfAddRun where
-- Device driver for half adder

-- import HDL.Hydra.Core.Lib
import Hydra.Core
import Hydra.Examples.HalfAdd

testdata :: [String]
testdata =
  ["0 0",
   "0 1",
   "1 0",
   "1 1"]

main :: IO ()
main = driver $ do
  useData testdata

-- Inputs
  x <- inputBit "x"
  y <- inputBit "y"

-- Circuit
  let (c,s) = halfAdd x y

-- Outputs
  outputBit "c" c
  outputBit "s" s

-- Run
  runCircuit
