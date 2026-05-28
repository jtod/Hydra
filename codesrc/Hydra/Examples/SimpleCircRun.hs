-- SimpleCircmRun:  device driver for SimpleCirc
-- Usage: $ ghc -e main Examples/SimpleCircRun

module Main where
import Hydra.Core
import Hydra.Examples.SimpleCirc

testdata1 :: [String]
testdata1=
------------------------------
--   x   y     expected output
------------------------------
  [ "0  0"   --  0
  , "0  1"   --  1
  , "1  0"   --  0
  , "1  1"   --  0
  ]

main :: IO ()
main = driver $ do
-- Input data
  useData testdata1

-- Inputs
  x <- inputBit "x"
  y <- inputBit "y"

-- Circuit
  let z = simpleCirc x y

-- Output ports
  outputBit "z" z

-- Run
  runCircuit
