-- HalfAddRun: simulation driver for half adder

module Main where
import HDL.Hydra.Core.Lib
import HalfAdd

testdata :: [String]
testdata =
  ["0 0",
   "0 1",
   "1 0",
   "1 1"]

main :: IO ()
main = driver $ do
  useData testdata

-- Input ports
  in_x <- inPortBit "x"
  in_y <- inPortBit "y"
  
-- Input signals
  let x = inbsig in_x
  let y = inbsig in_y

-- CIrcuit
  let (c,s) = myHalfAdd x y

-- Output ports
  out_c <- outPortBit "c" c
  out_s <- outPortBit "s" s

  runSimulation
