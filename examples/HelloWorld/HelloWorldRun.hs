-- File: examples/HelloWorld/HelloWorldRun.hs   (see Hydra/README.md)
-- Simulation driver for HelloWorld circuit
-- To run a simulation, enter:  ghc -e main HelloWorldRun

module Main where
import HDL.Hydra.Core.Lib
import HelloWorld
last line if first piece

first line in second piece
main :: IO ()
main = do
  helloRun testdata

testdata :: [[Int]]
testdata =
------------------------------
--   x   y     expected output (ref:datalabels)
------------------------------
  [ [0,  0]   --  0
  , [0,  1]   --  0
  , [1,  0]   --  0
  , [1,  1]   --  1
  ]

helloRun input = runAllInput input output
  where
-- Extract input signals
    x = getbit input 0
    y = getbit input 1
-- The circuit to be simulated
    z = hello x y
-- Format the output
    output =
      [string "  x=", bit x,
       string "  y=", bit y,
       string "  output z=", bit z
      ]
