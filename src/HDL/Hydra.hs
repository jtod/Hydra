module HDL.Hydra where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

hydraVersion :: String
<<<<<<< HEAD
hydraVersion = "3.4.16dev17"
=======
hydraVersion = "3.4.15edited-but-not-installed"
>>>>>>> 0f019b698764ae7a30fec5120de54e8f99c591c5

printVersion :: IO ()
printVersion = putStrLn ("Hydra version " ++ hydraVersion)
