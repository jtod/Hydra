-- HelloWorldRun.hs:  simulation driver for HelloWorld circuit

module Main where
import HDL.Hydra.Core.Lib
import HelloWorld

testdata1 :: [String]
testdata1=
------------------------------
--   x   y     expected output
------------------------------
  [ "0  0"   --  0
  , "0  1"   --  0
  , "1  0"   --  0
  , "1  1"   --  1
  ]

main :: IO ()
main = driver $ do
  useData testdata1

-- Input ports
  in_x <- inPortBit "x"
  in_y <- inPortBit "y"

-- Input signals
  let x = inbsig in_x
  let y = inbsig in_y

-- Circuit
  let z = hello x y

-- Formatted output
  format [string "x=", bit x,
          string "  y=", bit y,
          string "   output = ", bit z,
          string "\n"
         ]

  runSimulation
