-- Count4Run: simulation driver for 4-bit binary counter
-- John O'Donnell, 2021

module Main where
import HDL.Hydra.Core.Lib
import Count4

-- The file Count4 defines several versions of a 4-bit counter.  They
-- are explained in the comments in Count4.hs.  The following driver
-- runs the circuit count4a.  You can try the different versions of
-- the circuit by replacing count4a by any of count4b count4c count4d
-- count4by3 count 4by3

test_data_1 =
  ["0", "0", "0", "0", "1",
   "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1",
   "0", "1",
   "0", "0", "0", "0", "1",
   "0"]

main :: IO ()
main = driver $ do

-- Input data
  useData test_data_1

-- Inports
  in_reset <- inPortBit "reset"
  in_y     <- inPortWord "y" 4

-- Input signals
  let reset = inbsig in_reset
  let y = inwsig in_y

-- Circuit
  let y = count4by3 reset

-- Format the output  
  format
    [string "Input reset = ", bit reset,
     string "  Output = ", bindec 4 y]

  runSimulation

