module Main where
import HDL.Hydra.Core.Lib
import RBG

main :: IO ()
main = runrbg test_data_1

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

test_data_1 :: [[Int]]
test_data_1 =
  [[1], [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0]]
