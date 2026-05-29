module Hydra.Examples.Mux1 where
import Hydra.Core

mux1 :: Bit a => a -> a -> a -> a
mux1 c x y = or2 (and2 (inv c) x) (and2 c y)
