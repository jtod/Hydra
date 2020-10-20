module Reg1 where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

reg1 :: CBit a => a -> a -> a
reg1 ld x = r
  where r = dff (mux1 ld r x)
