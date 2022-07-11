-- Add4Run: simulation driver for add4 circuit
-- This file is part of Hydra.  https://github.com/jtod/Hydra
-- John T. O'Donnell, 2022

module Main where
import HDL.Hydra.Core.Lib
import Add4

-- Each test has input data "c x y" where c is the carry input (must
-- be 0 or 1) and x and y are integers between 0 and 15.  A number of
-- such tests are provided; they will run on successive clock cycles.

testdata1 :: [String]
testdata1 =
--------------------------------------------------
--  c   x   y    -- name of signal
--------------------------------------------------
  ["0   5   8",  -- inputs for clock cycle 0
   "0   7   3",  -- inputs for clock cycle 1
   "0   8  12",  -- inputs for clock cycle 2
   "0   8   1",  -- inputs for clock cycle 3
   "1  12   1",  -- inputs for clock cycle 4
   "1   2   3",  -- inputs for clock cycle 5
   "1  15  15"]  -- inputs for clock cycle 6

main :: IO ()
main = driver $ do
  useData testdata1

-- Inputs
  cin <- inPortBit "cin"
  x <- inPortWord "x" 4
  y <- inPortWord "y" 4

-- Circuit
  let (cout,s) = add4 cin x y

-- Outputs
  out_cout <- outPortBit "cout" cout
  out_s    <- outPortWord "s" s

-- Run
  runSimulation
