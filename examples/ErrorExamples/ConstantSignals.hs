module ConstSignalErrors where

-- Standard imports for typical circuit
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

-- Suppose we want a circuit whose input is a word, and whos output
-- is that word concatenated with a 4-bit word representing 5.

wrongExample :: CBit a => [a] -> [a]
wrongExample x = y
  where
    y = x ++ [0, 1, 0, 1]

correctExample :: CBit a => [a] -> [a]
correctExample x = y
  where
    y = x ++ [zero, one, zero, one]


