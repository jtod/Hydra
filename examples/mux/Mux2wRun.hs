module Main where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

main :: IO ()
main = runmux2w testdata

testdata :: [[Int]]
testdata =
  [[1,  39,  21,  82,  43],
   [0,  18,  74, 123, 280],
   [3,   1, 200,  35, 199],
   [1,   1,   3,   5,   7],
   [2, 200, 201, 202, 203]]
   
runmux2w input = runAllInput input output
  where
    c = getbit2  input 0
    p = getbin 8 input 1
    q = getbin 8 input 2
    r = getbin 8 input 3
    s = getbin 8 input 4
    x = mux2w c p q r s
    output =
      [string "c = ", bit (fst c), bit (snd c),
       string "  p =", bindec 4 p,
       string "  q =", bindec 4 q,
       string "  r =", bindec 4 r,
       string "  s =", bindec 4 s,
       string "  x =", bindec 4 x]
