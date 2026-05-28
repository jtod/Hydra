module Hydra.Examples.Reg1 where
import Hydra.Core
import Hydra.Examples.Mux1

reg1 :: CBit a => a -> a -> a
reg1 ld x = r
  where r = dff (mux1 ld r x)
