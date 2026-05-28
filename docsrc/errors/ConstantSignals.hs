-- ConstantSignals: illustrate right and wrong way to define constant
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module ConstSignalErrors where
import HDL.Hydra.Core.Lib

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


