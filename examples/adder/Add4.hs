-- Add4: circuit add4 adds two 4-bit words with carry
-- This file is part of Hydra, see Hydra/README.md for copyright and license

module Add4 where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

-- add4 is a 4-bit binary adder circuit.  The adder's inputs are a
-- carry input and two words.  The circuit outputs a carry output and
-- a sum word.  The fullAdd circuit is imported from the Combinational
-- library.  The definition has a separate equation for every bit
-- position.  This is the simplest way to define a word adder, but a
-- more general method is to use the scanr combinator.

-- Inputs:
--   cin is the carry input bit
--   x = [x0,x1,x2,x3] is a 4-bit input word
--   y = [y0,y1,y2,y3] is a 4-bit input word
-- Outputs:
--   c0 is the carry output bit
--   s = [s0,s1,s2,s3] is the 4-bit sum output word

add4 :: Bit a => a -> [a] -> [a] -> (a,[a])
add4 cin [x0,x1,x2,x3] [y0,y1,y2,y3]= (c0, [s0,s1,s2,s3])
  where
    (c0,s0) = fullAdd (x0,y0) c1
    (c1,s1) = fullAdd (x1,y1) c2
    (c2,s2) = fullAdd (x2,y2) c3
    (c3,s3) = fullAdd (x3,y3) cin
