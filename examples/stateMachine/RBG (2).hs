module RBG where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

{-
If reset=1 the machine goes to initial state, which is red.
Otherwise the state transitions are:
  red -> blue
  blue -> green
  green -> red
-}

stateMachine reset = (red,blue,green)
  where
    red = dff (or2 reset green)
    blue = dff (and2 reset' red)
    green = dff (and2 reset' blue)
    reset' = inv reset
