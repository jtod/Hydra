-- HalfAdd: define a half adder circuit
-- This file is part of Hydra, see Hydra/README.md for copyright and license

module HalfAdd where
import HDL.Hydra.Core.Lib

-- The half adder adds two bits x+y and produces a 2-bit result
-- (carry,sum).  This definition also appears in the standard circuit
-- libraries.  Note that there aren't any imports for circuits; the
-- only import is for the core library.

halfAdd :: Bit a => a -> a -> (a,a)
halfAdd x y = (and2 x y, xor2 x y)
