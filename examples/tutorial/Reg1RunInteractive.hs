-- Reg1Run: simulate a 1-bit register circuit
-- Copyright (C) 2021 John T. O'Donnell.  This file is part of Sigma16.
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

-- This is an example of how to simulate a small circuit
-- interactively.  The circuit (myreg1) uses individual bits.  For an
-- example that also uses words, see RegRun.  See the User Guide and
-- Sigma16/src/circuits/README.

-- ghci                      -- start ghci and initialize using .ghci
-- :load Reg1Run    -- load the simulation driver
-- :main                     -- run it: launch the main program

module Main where
import HDL.Hydra.Core.Lib   -- the hardware description language
import Reg1                 -- definition of the circuit to be simulated

main :: IO ()
main = driver $ do

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
