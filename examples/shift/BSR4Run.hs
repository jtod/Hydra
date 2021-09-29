-- BSR4Run: simulation driver for 4-bit bidirectional shift register
-- This file is part of Hydra.  John O'Donnell, 2021.  See Hydra/README

module Main where
import HDL.Hydra.Core.Lib
import BSR4

-- The circuit performs an operation determined by the operation code,
-- which is represented as a pair of bits (a,b)

--   00  no operation
--   01  load input word x
--   10  shift right
--   11  shift left

testdata :: [String]
testdata =
-------------------------------------
--  (a,b)  l  r   x      effect    y
-------------------------------------
  [ "0 1   0  0  9"   -- load 9    0
  , "0 0   0  0  0"   -- nop       9
  , "1 1   0  0  0"   -- shl 0     9
  , "1 1   0  1  0"   -- shl 1     2
  , "0 0   0  0  0"   -- nop       5
  , "0 1   0  0  4"   -- load 4    5
  , "1 0   1  0  0"   -- shr 1     4
  , "1 0   0  0  0"   -- shr 0     a
  , "1 0   0  0  0"   -- shr 0     5
  , "1 0   1  0  0"   -- shr 1     2
  , "0 0   0  0  0"   -- nop       9
  ]

main :: IO ()
main = driver $ do
  
-- Input data
  useData testdata

-- Input ports
  in_a <- inPortBit "a"
  in_b <- inPortBit "b"
  in_l <- inPortBit "l"
  in_r <- inPortBit "r"
  in_x <- inPortWord "x" 4

-- Input signals
  let a = inbsig in_a
  let b = inbsig in_b
  let l = inbsig in_l
  let r = inbsig in_r
  let x = inwsig in_x

-- Connect the inputs and output signals to the circuit
  let (lo,ro,y) = bsr4 (a,b) l r x

-- Format the output signals
  format
    [string "op=", bit a, bit b,
     string " l=", bit l,
     string " r=", bit r,
     string " x=", hex x,
     string "   Output lo=", bit lo,
     string " ro=", bit ro,
     string " y=", hex y]

-- Run the circuit on the inputs
  runSimulation
