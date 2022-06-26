-- SimDriverRun: demonstrate a simulation driver
-- This file is part of Hydra, see Hydra/README.md for copyright and license

module Main where
import HDL.Hydra.Core.Lib

-- Demonstrate a basic simulation driver, with input data of type
-- [[Int]] and with conversions for bits, binary, two's complement,
-- etc.  The simulation driver merely collects input signals (showing
-- how to define input signals from the test data) and outputs them
-- (showing how to format and output signals).  There isn't a circuit
-- here, but see other examples for simulation drivers that
-- incorporate a circuit.

main = do
  putStrLn "runSimDriver"
  runSimDriver inputdata

inputdata =
---------------------
--  0  1   2      3     <- these are the "column numbers"
---------------------
  [[0, 2, 35,     25],   -- inputs for cycle 0
   [0, 0,  9,    -25],   -- inputs for cycle 1
   [1, 1, 18, -30000],   -- inputs for cycle 2
   [0, 3, 27,    128],   -- inputs for cycle 3
   [1, 2, 51,    -42],   -- inputs for cycle 4
   [0, 1, 17,     -1]]   -- inputs for cycle 5

runSimDriver input = runAllInput input output
  where

-- Extract input signals from the input data
    a  = getbit    input 0   -- bit
    pq = getbit2   input 1   -- bit pair, i.e. 2 bits
    b  = getbin  8 input 2   -- 8 bit binary natural number
    c  = gettc  16 input 3   -- 16-bit two's complement integer

-- There is just a trivial circuit here
    x = inv a

-- Format the output
    output =
      [string "a = ", bit a,
       string " (p,q) = ", bit (fst pq), bit (snd pq),
       string " b = ", bindec 3 b,
       string " c = ", tcdec 7 c,
       string " (hex: ", hex c, string ")",
       string "  x=", bit x,
       fmtIf x
         [string " Yes! x is true"]
         [string " No! x is false"]
      ]
