-- HalfAdd: define a half adder circuit

module HalfAdd where
import HDL.Hydra.Core.Lib

-- The half adder adds two bits x+y and produces a 2-bit result
-- (carry,sum).  This definition also appears in the standard circuit
-- libraries.  Note that there aren't any imports for circuits; the
-- only import is for the core library.

-- The circuit is named myHalfAdd to avoid confusion with the halfAdd
-- circuit in the Hydra library (its definition is identical to this
-- one).

myHalfAdd :: Bit a => a -> a -> (a,a)
myHalfAdd x y = (and2 x y, xor2 x y)
