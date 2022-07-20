-- RBGrun: simulation driver for state machine
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module Main where
import HDL.Hydra.Core.Lib
import RBG

main :: IO ()
main = rbgRun testData1

-- The test data sets reset=1 in the first clock cycle, and then keeps
-- reset=0 thereafter.

testData1 :: [String]
testData1 =
  ["1 2 ", "0 3 ", "0", "0", "0", "0", "0", "0",
   "0", "0", "0", "0", "0", "0", "0"]

rbgRun :: [String] -> IO ()
rbgRun xs = driver $ do

-- Input data  
  useData xs

-- Inputs
  reset <- inputBit "reset"

-- Circuit
  let (r,b,g) = stateMachine reset

-- Outputs
  outputBit "r" r
  outputBit "b" b
  outputBit "g" g

-- Run
  runSimulation
