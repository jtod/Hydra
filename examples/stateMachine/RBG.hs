-- RBG: state machine circuit
-- This file is part of Hydra, see Hydra/README.md for copyright and license

module RBG where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

-- The stateMachine circuit has an internal state, which is one of
-- Red, Blue, Green.  If reset=1 the machine goes to initial state,
-- which is red.  It performs a state transition:
--   red -> blue
--   blue -> green
--   green -> red

stateMachine reset = (red,blue,green)
  where
    red = dff (or2 reset green)
    blue = dff (and2 reset' red)
    green = dff (and2 reset' blue)
    reset' = inv reset
