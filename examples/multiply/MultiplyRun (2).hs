-- MultiplyRun: simulation driver for multiply circuit
-- This file is part of Hydra, see Hydra/README.md for copyright and license

module Main where
import HDL.Hydra.Core.Lib
import Multiply

main :: IO ()
main = do
  run_multiply mult_test_data_1

-- Test data.  In the first cycle, 

mult_test_data_1 :: [[Int]]
mult_test_data_1 =
--     start  x    y
--     ~~~~~~~~~~~~~~
       [[1,  50,  75],  -- start=1 to multiply 50 * 75, expect 3750
        [0,   0,   0],  -- start=0 to give circuit time
        [0,   0,   0],  -- a number of clock cycles are needed
        [0,   0,   0],  -- give it another cycle
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [1, 100, 100],  -- start=1 to multiply 100*100, expect 10000
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [1, 100, 100],  -- start=1 to multiply 100*100, expect 10000
        [0,   0,   0],  -- working on 100*100
        [0,   0,   0],  -- working on 100*100
        [1,   2,   3],  -- abort and start multiplying 2*3
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0]]

-- Simulation driver

run_multiply :: [[Int]] -> IO ()
run_multiply input = runAllInput input output
  where
-- Size parameter
    k = 8
-- Extract input signals from readable input
    start = getbit   input 0
    x     = getbin k input 1
    y     = getbin k input 2
-- Connect the circuit to its inputs and outputs
    (rdy,prod,rx,ry,s) = multiply k start x y
-- Format the signals for output
    output =
      [string "Input: ",
       bit start, bindec 4 x, bindec 4 y,
       string "  Output: ",
       bit rdy, bindec 6 prod, bindec 4 rx, bindec 6 ry,
       bindec 6 s]
