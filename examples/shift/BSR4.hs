-- BSR4: 4-bit bidirectional shift register
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module BSR4 where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

-- The bsr4 circuit has a state consisting of a 4-bit word, which is
-- initially 0000.  It can shift its state to the left or right, it
-- can load a word into its state, and it can leave its state
-- unchanged.  An operation code input determines which operation to
-- perform.

-- The inputs are:
--   op determines what should be done to the state
--   li (left input) is a bit that can be shifted into the leftmost position
--   ri (right input) is a bit that can be shifted into the rightmost position
--   x is a 4-bit word that can be loaded into the state

-- The outputs are
--   left output (bit coming out of the leftmost bit of the state
--   right output (bit coming out of the rightmost bit of the state
--   the current value of the state

-- The circuit performs an operation determined by the operation code:
--   0  no operation
--   1  load input word x
--   2  shift right
--   3  shift left

-- The circuit is implemented using a building block called srb.

bsr4 :: CBit a => (a,a) -> a -> a -> [a] -> (a,a,[a])
bsr4 op li ri [x0,x1,x2,x3] = (y0, y3, [y0,y1,y2,y3])
  where
     y0 = srb op li y1 x0
     y1 = srb op y0 y2 x1
     y2 = srb op y1 y3 x2
     y3 = srb op y2 ri x3

-- The srb (shift register bit) circuit does the work for one bit
-- position in the shift register.

srb :: CBit a => (a,a) -> a -> a -> a -> a
srb op li ri x = y
  where y = dff (mux2 op y x li ri)
