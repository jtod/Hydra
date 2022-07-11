-- SimpleCirc -- A minimal example of a circuit description
-- This file is part of Hydra.  https://github.com/jtod/Hydra
-- John T. O'Donnell, 2022

module SimpleCirc where
import HDL.Hydra.Core.Lib

-- Define a circuit "simpleCirc" which takes two input bits and outputs
-- their logical conjunction, using an and2 logic gate

simpleCirc :: Bit a => a -> a -> a   -- interface: two inputs, one output
simpleCirc x y = and2 x y            -- circuit is just an and2 logic gate
