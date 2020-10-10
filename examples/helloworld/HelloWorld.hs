-- HelloWorld: simplest example of a circuit definition
-- This file is part of Hydra, see Hydra/README.md for copyright and license

-- To run the simulation, enter either of these commands:
--   hydra HelloWorldRun           -- README.md gives bash alias for "hydra"
--   ghc -e main HelloWorldRun     -- execute "main" in file HelloWorldRun.hs

module HelloWorld where     -- this module defines and exports the circuit
import HDL.Hydra.Core.Lib   -- import the Hydra core library

-- Define a circuit "hello" which takes two input bits and outputs
-- their logical conjunction (using an and2 logic gate)

hello :: Bit a => a -> a -> a   -- the type of the circuit gives its interface
hello x y = and2 x y            -- the implementation uses the and2 logic gate
