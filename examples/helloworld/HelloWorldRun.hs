-- HelloWorldRun: simplest example of a simulation driver
-- This file is part of Hydra, see Hydra/README.md for copyright and license

-- To run the simulation, enter either of these commands:
--   hydra HelloWorldRun          -- README.md gives bash alias for "hydra"
--   ghc -e main HelloWorldRun    -- execute "main" in file HelloWorldRun.hs

module Main where          -- a simulation driver is an executable Main program
import HDL.Hydra.Core.Lib  -- the Hydra core library defines the language
import HelloWorld          -- import the circuit to simulate

-- The main program is an IO operation of type IO () which executes
-- when you load this module and run "main".

main :: IO ()
main = do            -- each line below gives an operation to perform
  helloRun testdata  -- run "helloRun" simulation driver on testdata

-- Define test data for the simulation.  The input data to helloRun is
-- written as a list of lists of Int, so the type is [[Int]].  The
-- outer list contains one element for each clock cycle; each element
-- of the outer list as a list of Ints giving the input signal values
-- for that cycle.  For example, testdata says that during cycle 0 the
-- inputs are 0,0, during cycle 1 the inputs are 0,1, and so on.

testdata :: [[Int]]
testdata =
  [ [0,  0]  -- inputs during clock cycle 0
  , [0,  1]  -- inputs during clock cycle 1
  , [1,  0]  -- inputs during clock cycle 2
  , [1,  1]  -- inputs during clock cycle 3
  ]

-- helloRun is a simulation driver: when you apply it to test data, it
-- will run the simulation and output the results.  The primary
-- purpose of a simulation driver is to format the inputs and outputs,
-- to make it easier to provide the input data and to read the
-- simulation output.

helloRun :: [[Int]] -> IO ()
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
