-- RBGrun: simulation driver for state machine
-- This file is part of Hydra, see Hydra/README.md for copyright and license

module Main where
import HDL.Hydra.Core.Lib
import RBG

main :: IO ()
main = do
  runrbg test_data_1

runrbg :: [[Int]] -> IO ()
runrbg input = runAllInput input output
  where
-- Extract input signals from readable input
    reset = getbit input 0
-- Connect the circuit to its inputs and outputs
    (r,b,g) = stateMachine reset
-- Format the signals for output
    output =
      [string "reset = ", bit reset,
       string "   rbg = ", bit r, bit b, bit g]

-- The test data sets reset=1 in the first clock cycle, and then keeps
-- reset=0 thereafter.

test_data_1 :: [[Int]]
test_data_1 =
  [[1], [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0]]
