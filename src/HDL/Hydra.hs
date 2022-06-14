module HDL.Hydra where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

hydraVersion :: String
hydraVersion = "3.4.11"

printVersion :: IO ()
printVersion = putStrLn ("Hydra version " ++ hydraVersion)
