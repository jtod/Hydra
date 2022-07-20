-- SimpleCirc: minimal example of a circuit specification
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module SimpleCirc where
import HDL.Hydra.Core.Lib

-- Define a circuit "simpleCirc" which takes two input bits and
-- outputs their logical conjunction, using an and2 logic gate.

-- To run the circuit simulation, see SimpleCircRun and
-- SimpleCircRunInteractive.

simpleCirc :: Bit a => a -> a -> a   -- interface: two inputs, one output
simpleCirc x y = and2 x y            -- circuit is just an and2 logic gate
