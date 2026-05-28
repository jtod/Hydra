-- SimpleCirc: minimal example of a circuit specification

module Hydra.Examples.SimpleCirc where
import Hydra.Core

-- Define a circuit "simpleCirc" which takes two input bits and
-- outputs their logical conjunction, using an and2 logic gate.
-- To run the circuit simulation, see SimpleCircRun and
-- SimpleCircRunInteractive.

simpleCirc :: Bit a => a -> a -> a  -- interface
simpleCirc x y = and2 (inv x) y     -- circuit
