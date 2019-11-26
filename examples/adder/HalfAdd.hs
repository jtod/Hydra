module HalfAdd where
import HDL.Hydra.Core.Lib

halfAdd :: Bit a => a -> a -> (a,a)
halfAdd x y = (and2 x y, xor2 x y)
