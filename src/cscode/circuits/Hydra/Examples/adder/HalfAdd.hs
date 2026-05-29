module Hydra.Examples.HalfAdd where

-- HalfAdd: add two bits, produce carry and sum
-- import HDL.Hydra.Core.Lib
import Hydra.Core

-- The half adder adds two bits x+y and produces a 2-bit result
-- (carry,sum).  This definition also appears in the standard circuit
-- libraries.  Note that there aren't any imports for circuits; the
-- only import is for the core library.

-- The circuit is named myHalfAdd to avoid confusion with the halfAdd
-- circuit in the Hydra library (its definition is identical to this
-- one).

halfAdd :: Bit a => a -> a -> (a,a)
halfAdd x y = (and2 x y, xor2 x y)
