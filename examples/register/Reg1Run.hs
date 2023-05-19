-- Reg1Run: simulation driver for reg1
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module Main where
import HDL.Hydra.Core.Lib
import Reg1

main :: IO ()
main = reg1Run testData

testData :: [String]
testData =
------------------------
--  ld  x       output
------------------------
  [ "1  1"  -- 0  output in cycle 0 is initial state
  , "0  0"  -- 1  state changed to 1 at tick between cycles 0/1
  , "0  1"  -- 1  no change
  , "0  0"  -- 1  no change
  , "1  0"  -- 1  still see 1 but at end of cycle, set state to 0
  , "0  0"  -- 0 during this cycle can see result of state change
  , "1  1"  -- 0 but set state to 1 on tick at end of cycle
  , "1  0"  -- 1 the 1 becomes visible in this cycle
  , "0  0"  -- 0 the 0 now becomes visible
  , "0  0"  -- 0 no change
  ]

reg1Run :: [String] -> IO ()
reg1Run xs = driver $ do

-- Input data
  useData xs
  
-- Input ports
  ld <- inputBit "ld"
  x  <- inputBit "x"

-- Circuit
  let y = reg1 ld x

  outputBit "y" y

-- Run
  runSimulation
