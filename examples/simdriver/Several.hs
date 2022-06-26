-- Several: demonstrate how to run simulations of several circuits in one file
-- John O'Donnell, 2021

module Main where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Register

------------------------------------------------------------------------
-- Running executable functions of type IO ()
------------------------------------------------------------------------

-- An executable program has type IO (), and both simulation drivers
-- defined below have that type.

-- The command ghc -e FUNCTION moduleName looks in the file
-- moduleName for FUNCTION, which must have type IO (), and ghc runs
-- it.  The commonest case is ghc -e main moduleName.

-- When you enter the command ghc -e main Several this tells ghc to
-- execute the function named main in the module Several.  The main
-- function consists of a sequence of operations that are executed
-- in sequence.  Each of these operations must have type IO ().  The
-- function putStrLn "some stuff" prints out the contents of the
-- argument "some stuff"

-- You can run both simulations by ghc -e main Several.  You can run
-- just the xor by entering ghc -e xorDriver Several, and you can
-- run just the register by ghc -e reg1Driver Several.

------------------------------------------------------------------------
-- Run simulations of two circuits: xor and reg1
------------------------------------------------------------------------

-- The main function prints a message, runs the xor simulation,
-- prints another message, and runs the reg1 simulation.  The
-- putStrLn messages are not essential, but they make it easier to
-- read the outputs.

main :: IO ()
main = do
  putStrLn "*** xor simulation ***"
  xorDriver
  putStrLn "\n\n*** reg1 simulation ***"
  reg1Driver

------------------------------------------------------------------------
-- Simulate an xor gate
------------------------------------------------------------------------

-- The first circuit we'll simulate is an xor gate.  This is already
-- defined in the Hydra libraries, so we don't need to import a
-- separate file defining the circuit.

xorTestData :: [String]
xorTestData =
  [ "0 0"
  , "0 1"
  , "1 0"
  , "1 1"
  ]

-- Every simulation driver should have the type IO ()

xorDriver :: IO ()
xorDriver = driver $ do

-- Input data
  useData xorTestData
  
-- Input ports
  in_x  <- inPortBit "x"
  in_y  <- inPortBit "y"

-- Input signals
  let x  = inbsig in_x
  let y  = inbsig in_y

-- Circuit
  let z = xor2 x y

-- Format the outputs
  format
    [string "Input x = ", bit x,
     string " y = ", bit y,
     string "   Output z = ", bit z]

-- Run the circuit on the inputs
  runSimulation

------------------------------------------------------------------------
-- Simulate reg1 circuit
------------------------------------------------------------------------

-- The second circuit we'll simulate is reg1.  This is already
-- defined in the Hydra libraries, so we don't need to import a
-- separate file defining the circuit.

reg1TestData :: [String]
reg1TestData =
------------------------
--  ld  x       output
------------------------
  [ "1  1"  -- 0  output in cycle 0 is initial state
  , "0  0"  -- 1  state changed to 1 at tick between cycles 0/1
  , "0  1"  -- 1  no change
  , "0  0"  -- 1  no change
  , "1  0"  -- 1  still see 1 but at end of cycle, set state to 0
  , "0  0"  -- 0 during this cycle can see result of state change
  , "1  1"  -- 0 but set state to 1 on tick at end of cycle
  , "1  0"  -- 1 the 1 becomes visible in this cycle
  , "0  0"  -- 0 the 0 now becomes visible
  , "0  0"  -- 0 no change
  ]

reg1Driver :: IO ()
reg1Driver = driver $ do

-- Input data
  useData reg1TestData
  
-- Input ports
  in_ld <- inPortBit "ld"
  in_x  <- inPortBit "x"

-- Input signals
  let ld = inbsig in_ld
  let x  = inbsig in_x

-- Circuit
  let y = reg1 ld x

-- Format the outputs
  format
    [string "Input ld = ", bit ld,
     string " x = ", bit x,
     string "   Output = ", bit y]

-- Run the circuit on the inputs  
  runSimulation
