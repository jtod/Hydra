-- File: examples/HelloWorld/HelloWorld.hs   (see Hydra/README.md)
-- Defines a simple circuit; see HelloWorldRun to simulate it

module HelloWorld where
import HDL.Hydra.Core.Lib

-- Define a circuit "hello" which takes two input bits and outputs
-- their logical conjunction, using an and2 logic gate

hello :: Bit a => a -> a -> a   -- the type of the circuit gives its interface
hello x y = and2 x y            -- the implementation uses an and2 logic gate
