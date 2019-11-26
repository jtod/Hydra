module Main where
import HDL.Hydra.Core.Lib
import HelloWorld

main :: IO ()
main = helloRun testdata

testdata :: [[Int]]
testdata =
  [ [0,  0]
  , [0,  1]
  , [1,  0]
  , [1,  1]
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
