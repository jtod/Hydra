-- SimpleCirc -- A minimal example of a circuit description
-- This file is part of Hydra. John O'Donnell, 2021.  See Hydra/README

module SimpleCirc where
import HDL.Hydra.Core.Lib

-- Define a circuit "simpleCirc" which takes two input bits and outputs
-- their logical conjunction, using an and2 logic gate

simpleCirc :: Bit a => a -> a -> a   -- interface: two inputs, one output
simpleCirc x y = and2 x y            -- implementation uses an and2 logic gate
