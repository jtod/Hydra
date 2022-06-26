-- Mux2wRun: test mux2w Circuits library
-- This file is part of Hydra.  John O'Donnell, 2021

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

testdata :: [String]
testdata =
-------------------------------------------------
--(ca,cb)    p    q    r    s        exected output
-------------------------------------------------
 [ "0 0     18   74  123  280"    -- expect p =  18
 , "0 1     39   21   82   43"    -- expect q =  21
 , "1 0    200  201  202  203"    -- expect r = 202
 , "1 1      1  200   35  199"    -- expect s = 199
 ]

main :: IO ()
main = driver $ do

-- Input data
  useData testdata

-- Input ports
  in_ca <- inPortBit "ca"
  in_cb <- inPortBit "cb"
  in_p  <- inPortWord "p" 8
  in_q  <- inPortWord "q" 8
  in_r  <- inPortWord "r" 8
  in_s  <- inPortWord "s" 8

-- Input signals
  let ca = inbsig in_ca
  let cb = inbsig in_cb
  let p  = inwsig in_p
  let q  = inwsig in_q
  let r  = inwsig in_r
  let s  = inwsig in_s
  
-- Circuit
  let x = mux2w (ca,cb) p q r s

-- Format the signals
  format
    [string "c = ", bit ca, bit cb,
     string "  p =", bindec 4 p,
     string "  q =", bindec 4 q,
     string "  r =", bindec 4 r,
     string "  s =", bindec 4 s,
     string "  x =", bindec 4 x]

-- Run the circuit on the inputs
  runSimulation


{-
    #
runmux2w input = runAllInput input output
  where
    c = getbit2  input 0
    p = getbin 8 input 1
    q = getbin 8 input 2
    r = getbin 8 input 3
    s = getbin 8 input 4
    x = mux2w c p q r s
    output =
      [string "c = ", bit (fst c), bit (snd c),
       string "  p =", bindec 4 p,
       string "  q =", bindec 4 q,
       string "  r =", bindec 4 r,
       string "  s =", bindec 4 s,
       string "  x =", bindec 4 x]
-
-}
