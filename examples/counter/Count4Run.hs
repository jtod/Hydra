-- Count4Run: simulation driver for 4-bit binary counter
-- This file is part of Hydra; see Hydra/README.md for copyright and license

module Main where
import HDL.Hydra.Core.Lib
import Count4

main :: IO ()
main = do
     putStrLn "Running counter4a on testdata_1"
     runCount4 count4a testdata_1
     putStrLn "\nRunning count4b on testdata_1"
     runCount4 count4b testdata_1
     putStrLn "\nRunning count4c on testdata_1"
     runCount4 count4c testdata_1
     putStrLn "\nRunning count4d on testdata_1"
     runCount4 count4d testdata_1
     putStrLn "\nRunning count4by2 on testdata_1"
     runCount4 count4by2 testdata_1
     putStrLn "\nRunning count4by3 on testdata_1"
     runCount4 count4by3 testdata_1

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


