module Main where
import HDL.Hydra.Core.Lib
import Count4

main :: IO ()
main =
  do putStrLn "Running version (a) on testdata_1"
     runCount4 count4a testdata_1
     putStrLn "Running version (b) on testdata_1"
     runCount4 count4b testdata_1

testdata_1 =
  [[0], [0], [0], [0], [1],
   [0], [0], [0], [0], [0], [0], [0], [0], [0], [0], [1],
   [0], [1],
   [0], [0], [0], [0], [1],
   [0]]

runCount4 circuit input = runAllInput input output
  where
    reset = getbit input 0
    y = circuit reset
    output =
      [string "Input reset = ", bit reset,
       string "  Output = ", bindec 4 y]


