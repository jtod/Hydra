-- HalfAddRun: simulation driver for half adder
-- This file is part of Hydra.  https://github.com/jtod/Hydra
-- John O'Donnell, 2022

module Main where
import HDL.Hydra.Core.Lib
import HalfAdd

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
  x <- inPortBit "x"
  y <- inPortBit "y"

-- Circuit
  let (c,s) = myHalfAdd x y

-- Outputs
  outPortBit "c" c
  outPortBit "s" s

-- Run
  runSimulation
