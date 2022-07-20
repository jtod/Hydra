-- SimpleCircRun:  simulation driver for SimpleCirc
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

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
  x <- inputBit "x"
  y <- inputBit "y"

-- Circuit
  let z = simpleCirc x y

-- Output ports
  outputBit "z" z

-- Run
  runSimulation
