-- HalfAddRun: simulation driver for half adder
-- This file is part of Hydra, see Hydra/README.md for copyright and license

module HalfAddRun where
import HDL.Hydra.Core.Lib
import HalfAdd

main :: IO ()
main = do
  run_halfAdd testdata

testdata :: [[Int]]
testdata =
  [[0, 0],
   [0, 1],
   [1, 0],
   [1, 1]]

-- Simulation driver for half adder

run_halfAdd :: [[Int]] -> IO ()
run_halfAdd input = runAllInput input output
  where
-- Extract input signals from simulation input
    x = getbit input 0
    y = getbit input 1
-- Connect the circuit to its inputs and outputs
    (c,s) = halfAdd x y
-- Format the simulation output
    output =
      [string "Input: x = ", bit x, string " y = ", bit y,
       string "  Output: c = ", bit c, string " s = ", bit s]
