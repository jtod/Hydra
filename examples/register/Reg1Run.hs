--Simulation driver for reg1
-- John O'Donnell, 2021

module Main where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Register

-- Using the reg1 circuit defined in the ydra libraries.  Here is the
-- definition:

--   reg1 :: CBit a => a -> a -> a
--   reg1 ld x = r
--     where r = dff (mux1 ld r x)

testdata :: [String]
testdata =
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

main :: IO ()
main = driver $ do

-- Input data
  useData testdata
  
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

{-
runReg1 input = runAllInput input outputQ
  where
-- Input signals
    ld = getbit input 0
    x  = getbit input 1
-- Circuit
    y = reg1 ld x
-- Format the input and output signals
    output =
      [string "Input ld = ", bit ld,
       string " x = ", bit x,
       string "   Output = ", bit y]
-}
