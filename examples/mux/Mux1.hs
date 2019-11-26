module Mux1 where
import HDL.Hydra.Core.Lib

-- mux1 is defined in the Hydra circuit libraries, so here the circuit
-- is called mymux1 to ensure that we're actually testing this
-- definition

mymux1 :: Bit a => a -> a -> a -> a
mymux1 c x y = or2 (and2 (inv c) x) (and2 c y)
