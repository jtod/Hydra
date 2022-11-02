-- Count4Run: simulation driver for 4-bit binary counter
-- This file is part of Hydra.  https://github.com/jtod/Hydra 
-- Copyright (c) 2022 John T. O'Donnell.  See Hydra/README

module Main where
import HDL.Hydra.Core.Lib
import Count4

-- The file Count4 defines several versions of a 4-bit counter.  They
-- are explained in the comments in Count4.hs.  The following driver
-- runs the circuit count4a.  You can try the different versions of
-- the circuit by replacing count4a by any of count4b count4c count4d
-- count4by3 count 4by3

main :: IO ()
main = do
     starline
     putStrLn "Running counter4a on testData1"
     runCount4 count4a testData1
     starline
     putStrLn "\nRunning count4b on testData1"
     runCount4 count4b testData1
     starline
     putStrLn "\nRunning count4c on testData1"
     runCount4 count4c testData1
     starline
     putStrLn "\nRunning count4d on testData1"
     runCount4 count4d testData1
     starline
     putStrLn "\nRunning count4by2 on testData1"
     runCount4 count4by2 testData1
     starline
     putStrLn "\nRunning count4by3 on testData1"
     runCount4 count4by3 testData1

starline = putStrLn ('\n' : take 70 (repeat '*'))

testData1 =
  ["0", "0", "0", "0", "1",
   "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1",
   "0", "1",
   "0", "0", "0", "0", "1",
   "0"]

-- Run counter circuit circ on test data x.  Use B as type for bit
-- signal
runCount4 :: (B -> [B]) -> [String] -> IO ()
runCount4 circ x = driver $ do

-- Input data
  useData x

-- Inputs
  reset <- inputBit "reset"

-- Circuit
  let y = circ reset

-- Outputs
  outputWord "y" y

-- Run
  runSimulation
