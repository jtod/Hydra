-- BSR4Run: simulation driver for 4-bit bidirectional shift register
-- This file is part of Hydra, see Hydra/README.md for copyright and license

module BSR4Run where
import HDL.Hydra.Core.Lib
import BSR4

main :: IO ()
main = do
  runBSR4 testdata

-- The circuit performs an operation determined by the operation code:
--   0  no operation
--   1  load input word x
--   2  shift right
--   3  shift left

testdata :: [[Int]]
testdata =
-----------------------------------
--  op l  r   x        op      y
-----------------------------------
  [[1, 0, 0,  9],  -- load 9   0
   [0, 0, 0,  0],  -- nop      9
   [3, 0, 0,  0],  -- shl 0    9
   [3, 0, 1,  0],  -- shl 1    2
   [0, 0, 0,  0],  -- nop      5
   [1, 0, 0,  4],  -- load 4   5
   [2, 1, 0,  0],  -- shr 1    4
   [2, 0, 0,  0],  -- shr 0    a
   [2, 0, 0,  0],  -- shr 0    5
   [2, 1, 0,  0],  -- shr 1    2
   [0, 0, 0,  0]]  -- nop      9

runBSR4 :: [[Int]] -> IO ()
runBSR4 input = runAllInput input output
  where
-- Extract input signals from the input data
    op = getbit2  input 0
    l  = getbit   input 1
    r  = getbit   input 2
    x  = getbin 4 input 3
-- Connect the inputs and output signals to the circuit
    (lo,ro,y) = bsr4 op l r x
-- Format the output signals
    output =
      [string "op=", bit (fst op), bit (snd op),
       string " l=", bit l,
       string " r=", bit r,
       string " x=", hex x,
       string "   Output lo=", bit lo,
       string " ro=", bit ro,
       string " y=", hex y]

