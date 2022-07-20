-- Reg1RunData: simulate a 1-bit register circuit with pre-specified input data
-- This file is part of Hydra.  John O'Donnell, 2021.  See Hydra/README

-- This is an example of how to simulate a small circuit using
-- pre-defined input data, not reading the inputs from the user.  The
-- presence of the "useData" statement tells Hydra to run the
-- simulation in batch mode with the specified input data.

module Main where
import HDL.Hydra.Core.Lib    -- the hardware description language
import Reg1                  -- definition of the circuit to be simulated

randomInputs, alwaysLoad1, alwaysLoad0, idle :: [String]
randomInputs = [ "1 1", "0 0", "0 0", "1 0", "0 0", "0 1", "1 1", "0 1", "0 0"]
alwaysLoad1 = ["1 1", "1 1", "1 1", "1 1", "1 1", "1 1"]
alwaysLoad0 = ["1 0", "1 0","1 0","1 0","1 0","1 0","1 0"]
idle = ["0 0", "0 0", "0 0", "0 0", "0 0", "0 0"]

main :: IO ()
main = driver $ do

-- The useData command specifies which data set to use  
  useData randomInputs
  
  -- Input ports
  in_ld <- inPortBit "ld"
  in_x  <- inPortBit "x"

  -- Input signals
  let ld = inbsig in_ld
  let x  = inbsig in_x
  
  -- The circuit myreg1 receives input signals and defines output signals
  let (r,q) = myreg1 ld x

  -- The output ports are interfaces to the output signals
  out_r <- outPortBit "r" r
  out_q <- outPortBit "q" q
  
  -- Run interactive simulation 
  runSimulation
  
