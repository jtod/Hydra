-- BSR4Run: simulation driver for 4-bit bidirectional shift register
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module Main where
import HDL.Hydra.Core.Lib
import BSR4

-- The circuit performs an operation determined by the operation code,
-- which is represented as a pair of bits (a,b)

--   00  no operation
--   01  load input word x
--   10  shift right
--   11  shift left

main :: IO ()
main = runBSR4 testData

testData :: [String]
testData =
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

runBSR4 :: [String] ->  IO ()
runBSR4 xs = driver $ do
  
-- Input data
  useData xs

-- Inputs
  a <- inputBit "a"
  b <- inputBit "b"
  l <- inputBit "l"
  r <- inputBit "r"
  x <- inputWord "x" 4

-- Circuit
  let (lo,ro,y) = bsr4 (a,b) l r x

  outputBit "lo" lo
  outputBit "ro" ro
  outputWord "y" y

-- Run the circuit on the inputs
  runSimulation
