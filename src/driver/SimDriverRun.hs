-- SimDriverRun: demonstrate a simulation driver
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module Main where
import HDL.Hydra.Core.Lib

-- Demonstrate a basic simulation driver, with conversions for bits,
-- binary, two's complement, etc.  The simulation driver merely
-- collects input signals (showing how to define input signals from
-- the test data) and outputs them (showing how to format and output
-- signals).  There isn't a circuit here, but see other examples for
-- simulation drivers that incorporate a circuit.

------------------------------------------------------------------------
-- Dev, clean up and move to Core/Driver. adapting code from M1/Driver

doCycle = do
  establishInputs (takeInputsFromList)
  runFormat
  advanceInPorts
  advanceOutPorts
  advanceFormat

-- End dev
------------------------------------------------------------------------

main = do
  putStrLn "runSimDriver"
  runDriver inputData


inputData :: [String]
inputData =
-------------------------------------------------
--   a   b   c       d
-------------------------------------------------
  [ "0   2   35      25"   -- inputs for cycle 0
  , "0   0    9     -25"   -- inputs for cycle 1
  , "1   1   18  -30000"   -- inputs for cycle 2
  , "0   3  127     128"   -- inputs for cycle 3
  , "1   2  251     -42"   -- inputs for cycle 4
  , "0   1   17      -1"   -- inputs for cycle 5
  ]

runDriver :: [String] -> IO ()  
runDriver xs = driver $ do

-- Input data
  useData xs

-- Inputs
  a <- inputBit "a"
  b <- inputWord "b" 2
  c <- inputWord "c" 8
  d <- inputWord "d" 16

-- There is just a trivial circuit here
  let x = inv a

-- Format the output
  format
    [string "a = ", bit a,
     string " b = ", bindec 1 b,
     string " c = ", tcdec 3 c,
     string " (c in hex: ", hex c,
     string " and in bits: ", bits c, string ")",
     string "\nd = ", tcdec 5 d, string " (hex: ", hex d, string ")",
     string " x=", bit x,
     fmtIf x
       [string " Yes! x is true"]
       [string " No! x is false"],
      string "\n"
    ]

-- Run the circuit
  runSimulation
