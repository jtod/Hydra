-- MultiplyRun: simulation driver for multiply circuit
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

-- Usage: $ ghc -e main MultiplyRun

-- To run the multiplier:
--   $ ghci
--   ghci> :load MultiplyRun
--   ghci> :main
--   hydra> run

module Main where
import HDL.Hydra.Core.Lib
import Multiply

main :: IO ()
main = multiplyDriver mult_test_data_1

mult_test_data_1 :: [String]
mult_test_data_1 =
--     start  x    y
--     ~~~~~~~~~~~~~~
       ["1    50   75",  -- start=1 to multiply 50 * 75, expect 3750
        "0    0     0",  -- start=0 to give circuit time
        "0    0     0",  -- a number of clock cycles are needed
        "0    0     0",  -- give it another cycle
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "1  100   100",  -- start=1 to multiply 100*100, expect 10000
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "1  100   100",  -- start=1 to multiply 100*100, expect 10000
        "0    0     0",  -- working on 100*100
        "0    0     0",  -- working on 100*100
        "1    2     3",  -- abort and start multiplying 2*3
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0"]

-- main :: Driver a
multiplyDriver :: [String] -> IO ()
multiplyDriver xs = driver $ do

-- Size parameter
  let k = 8

-- Input data  
  useData xs

-- Inputs
  start <- inputBit "start"
  x     <- inputWord "x" k
  y     <- inputWord "y" k

-- Circuit
  let (rdy,prod,rx,ry,s) = multiply k start x y

-- Outputs
  outputBit "rdy" rdy
  outputWord "prod" prod
  outputWord "x" x
  outputWord "y" y
  outputWord "s" s
  
-- Run
  runSimulation
