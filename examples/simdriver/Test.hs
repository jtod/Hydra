-- SimDriverRun: demonstrate a simulation driver
-- This file is part of Hydra, see Hydra/README.md for copyright and license

module Main where
import HDL.Hydra.Core.Lib

-- Demonstrate a basic simulation driver, with conversions for bits,
-- binary, two's complement, etc.  The simulation driver merely
-- collects input signals (showing how to define input signals from
-- the test data) and outputs them (showing how to format and output
-- signals).

inputdata :: [String]
inputdata =
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

main :: IO ()  
main = driver $ do

-- Input data
  useData inputdata      

-- Input ports
  in_a <- inPortBit "a"
  in_b <- inPortWord "b" 2
  in_c <- inPortWord "c" 8
  in_d <- inPortWord "d" 16

-- Input signals
  let a = inbsig in_a
  let b = inwsig in_b
  let c = inwsig in_c
  let d = inwsig in_d

-- There is just a trivial circuit here
  let x = inv a

-- Format the output
  format
    [string "a = ", bit a,
     string " b = ", bindec 1 b,
     string " c = ", tcdec 3 c,
     string " (c in hex: ", hex c,
     string " and in bits: ", bits c, string ")",
     string " d = ", tcdec 5 d, string " (hex: ", hex d, string ")",
     string "\nFOOBAR", bindec 10 c, string "BAX", bindec 1 c, string "X\n",
     string " x=", bit x,
     fmtIf x
       [string " Yes! x is true"]
       [string " No! x is false"]
    ]

-- Run the circuit
  runSimulation

  
{-
    a  = getbit    input 0   -- bit
    pq = getbit2   input 1   -- bit pair, i.e. 2 bits
    b  = getbin  8 input 2   -- 8 bit binary natural number
    c  = gettc  16 input 3   -- 16-bit two's complement integer



-- Extract input signals from the input data

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
-}
