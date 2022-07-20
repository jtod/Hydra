-- Print Hydra version

module Main where
import HDL.Hydra

main :: IO ()
main = putStrLn ("Hydra version: " ++ HDL.Hydra.hydraVersion)
