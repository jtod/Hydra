module HelloWorld where
import HDL.Hydra.Core.Lib

hello :: Bit a => a -> a -> a
hello x y = and2 x y
