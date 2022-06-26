-- Add4Run: simulation driver for add4 circuit

module Main where
import HDL.Hydra.Core.Lib
import Add4

-- Each test has input data "c x y" where c is the carry input (must
-- be 0 or 1) and x and y are integers between 0 and 15.  A number of
-- such tests are provided; they will run on successive clock cycles.

test1 :: [String]
test1 =
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
  useData test1

-- Input ports
  in_cin <- inPortBit "cin"
  in_x   <- inPortWord "x" 4
  in_y   <- inPortWord "y" 4

-- Input signals
  let cin = inbsig in_cin
  let x   = inwsig in_x
  let y   = inwsig in_y

-- Circuit
  let (cout,s) = add4 cin x y

-- Format the output
  format
    [string "Inputs: ", string "  x =", bindec 3 x,
     string "  y =", bindec 3 y,
     string "  cin = ", bit cin,
     string "    Outputs: cout = ", bit cout,
     string "  s =", bindec 3 s,
     string "\n"]

  runSimulation
