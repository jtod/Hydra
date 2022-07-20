-- Mux2wRun: test mux2w circuit
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module Main where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

-- The Hydra circuit library contains a circuit called mux2w, which is
-- a multiplexer with 2 input control bits and 4 input data words.
-- This file uses a simulation driver and test data to demonstrate the
-- circuit.

-- The mux2w multiplexer uses the control bits to select one of the
-- data words and output it; the other data inputs are discarded.
-- Suppose c is a pair of bits, so c :: Bit a => (a,a).  Also suppose
-- p,q,r,s are words of type Bit a => [a]. Then mux2 c p q r s has the
-- value:

-- Note: c = (ca,cb)#
--    if c = (0,0) then p
--    if c = (0,1) then q
--    if c = (1,0) then r
--    if c = (1,1) then s

main :: IO ()
main = runmux2w testData

testData :: [String]
testData =
-------------------------------------------------
--(ca,cb)    p    q    r    s        exected output
-------------------------------------------------
 [ "0 0     18   74  123  280"    -- expect p =  18
 , "0 1      1    3    5    7"    -- expect q =   3
 , "0 1     39   21   82   43"    -- expect q =  21
 , "1 0    200  201  202  203"    -- expect r = 202
 , "1 1      1  200   35  199"    -- expect s = 199
 , "1 1      1    3    5   7"     -- expect s =   7
 ]

runmux2w :: [String] -> IO ()
runmux2w xs = driver $ do

-- Input data
  useData xs

-- Input ports
  ca <- inputBit "ca"
  cb <- inputBit "cb"
  p  <- inputWord "p" 8
  q  <- inputWord "q" 8
  r  <- inputWord "r" 8
  s  <- inputWord "s" 8

-- Circuit
  let x = mux2w (ca,cb) p q r s

-- Outputs
  outputWord "x" x

-- Run
  runSimulation
