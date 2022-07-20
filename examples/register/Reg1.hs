-- Reg1: register
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module Reg1 where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

-- reg1 is a register circuit with 1 bit of state.  It takes two
-- inputs: ld is a load control, and x is a data bit.  At a clock
-- tick, the register replaces its state with x if ld=1, but retains
-- its previous state if ld=0.  The register outputs its state
-- continuously.  This circuit is defined in the Registers library, so
-- normally you would import that library rather than including this
-- definition.

reg1 :: CBit a => a -> a -> a
reg1 ld x = r
  where r = dff (mux1 ld r x)
