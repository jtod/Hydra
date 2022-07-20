-- Mux1wRun: simulation driver for mux1w circuit
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module Main where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational  -- defines mux1w circuit

-- The mux1w circuit is defined in HDL.Hydra.Circuits.Combinational.
-- Here is the circuit definition (commented out to avoid redefining
-- it in this file):

-- mux1w :: Bit a => a -> [a] -> [a] -> [a]
-- mux1w c x y = map2 (mux1 c) x y

main :: IO ()
main = mux1wRun testData1

testData1 =
  [ "0 3 9",  -- expect 3
    "1 3 9",  -- expect 9
    "0 1 2",  -- expect 1
    "1 1 2"]  -- expect 2

mux1wRun :: [String] -> IO ()
mux1wRun xs = driver $ do

-- Input data supplied, so it will run in Batch mode
  useData xs

-- Inputs
  c <- inputBit "c"
  x <- inputWord "x" 4
  y <- inputWord "y" 4

-- Circuit
  let r = mux1w c x y

-- Outputs
  outputWord "r" r

-- Run
  runSimulation
