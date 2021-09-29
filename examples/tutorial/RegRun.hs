-- RegRun: simulate a word register circuit
-- This file is part of Hydra.  John O'Donnell, 2021.  See Hydra/README

-- This is an example of how to simulate a small circuit
-- interactively, where the circuit (myreg) uses words as well as
-- individual bits

module Main where
import HDL.Hydra.Core.Lib   -- the hardware description language
import Reg                 -- definition of the circuit to be simulated

main :: IO ()
main = driver $ do

  -- Input ports
  in_ld <- inPortBit "ld"
  in_x <- inPortWord "x" 4

  -- Input signals  
  let ld = inbsig in_ld
  let x = inwsig in_x
  
  -- Circuit defines output signals
  let (r,q) = myreg4 ld x

  -- Output ports  
  out_r <- outPortWord "r" r
  out_q <- outPortWord "q" q

  -- Run simulation
  runSimulation
