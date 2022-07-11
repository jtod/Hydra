-- SimpleCircdRun.hs:  simulation driver for SimpleCirc
-- This file is part of Hydra.  https://github.com/jtod/Hydra
-- John T. O'Donnell, 2022

-- Usage:  $ ghc -e main SimpleCircRun

module Main where
import HDL.Hydra.Core.Lib
import SimpleCirc

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
-- Input data
  useData testdata1

-- Inputs
  x <- inPortBit "x"
  y <- inPortBit "y"

-- Circuit
  let z = simpleCirc x y

-- Output ports
  outPortBit "z" z

-- Run
  runSimulation
