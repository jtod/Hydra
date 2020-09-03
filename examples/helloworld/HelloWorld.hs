-- HelloWorld: simplest example of a circuit
-- This file (HelloWorld) defines the circuit
-- To run a simulation, enter hydra HelloWorldRun
-- This file is part of Hydra, see Hydra/README.md

module HelloWorld where
import HDL.Hydra.Core.Lib

-- Define a circuit "hello" which takes two input bits and outputs
-- their logical conjunction (using an and2 logic gate)

hello :: Bit a => a -> a -> a   -- type of the circuit gives its interface
hello x y = and2 x y            -- implementation uses the and2 logic gate
