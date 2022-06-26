-- Mux2wRun: test mux2w Circuits library
-- This file is part of Hydra, see Hydra/README.md for copyright and license

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

--    if c = (0,0) then p
--    if c = (0,1) then q
--    if c = (1,0) then r
--    if c = (1,1) then s

main :: IO ()
main = do
  runmux2w testdata

testdata :: [[Int]]
testdata =
-------------------------------------------------
--  c    p    q    r    s        exected output
-------------------------------------------------
  [[1,  39,  21,  82,  43],   -- expect q =  21
   [0,  18,  74, 123, 280],   -- expect p =  18
   [3,   1, 200,  35, 199],   -- expect s = 199
   [1,   1,   3,   5,   7],   -- expect q =   3
   [2, 200, 201, 202, 203]]   -- expect r = 202
   
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
