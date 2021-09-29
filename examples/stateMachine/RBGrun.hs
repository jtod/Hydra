-- RBGrun: simulation driver for state machine
-- This file is part of Hydra.  John O'Donnell, 2021.  See Hydra/README

module Main where
import HDL.Hydra.Core.Lib
import RBG

-- The test data sets reset=1 in the first clock cycle, and then keeps
-- reset=0 thereafter.

test_data_1 :: [String]
test_data_1 =
  ["1 2 ", "0 3 ", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"]

main :: IO ()
main = driver $ do

-- Input data  
  useData test_data_1

-- Input ports
  in_reset <- inPortBit "reset"

-- Input signals
  let reset = inbsig in_reset

-- Connect circuit to input and output signals
  let (r,b,g) = stateMachine reset

-- Format the simulation output
  format
    [string "reset = ", bit reset,
     string "   rbg = ", bit r, bit b, bit g]

-- Run the circuit
  runSimulation
