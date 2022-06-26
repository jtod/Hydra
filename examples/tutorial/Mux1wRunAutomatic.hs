-- Mux1wRunAutomatic: simulate they Hydra mux1w circuit, automatic output
-- This file is part of Hydra.  John O'Donnell, 2021, see Hydra/READKE

module Main where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational  -- defines mux1w circuit

-- The mux1w circuit is defined in HDL.Hydra.Circuits.Combinational.
-- Here is the circuit definition (commented out to avoid redefining
-- it in this file):

-- mux1w :: Bit a => a -> [a] -> [a] -> [a]
-- mux1w c x y = map2 (mux1 c) x y

testdata1 =
  [ "0 3 9",
    "1 3 9",
    "0 1 2",
    "1 1 2"]

main :: IO ()
main = driver $ do

-- Input data supplied, so it will run in Batch mode
  
  useData testdata1
  -- Input ports
  in_c <- inPortBit "c"
  in_x <- inPortWord "x" 4
  in_y <- inPortWord "y" 4

  -- Input signals
  let c = inbsig in_c
  let x = inwsig in_x
  let y = inwsig in_y

-- The circuit myreg1 receives input signals and defines output signals
  let r = mux1w c x y

-- Output ports  
  out_r <- outPortWord "r" r

-- There is no format specified, so the output will be produced
-- automatically using the ports

  runSimulation
