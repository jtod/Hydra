module Main where
import HDL.Hydra.Core.Lib
import Add4

main :: IO ()
main = run_add4 testdata

-- Each test is a list of [c, x, y] where c is the carry input (must
-- be 0 or 1) and x and y are integers between 0 and 15.  A number of
-- such tests are provided.

testdata :: [[Int]]
testdata =
--  c   x   y
  [[0,  5,  8],
   [0,  7,  3],
   [0,  8, 12],
   [0,  8,  1],
   [1, 12,  1],
   [1,  2,  3],
   [1, 15, 15]]

run_add4 :: [[Int]] -> IO ()
run_add4 input = runAllInput input output
  where
-- Extract input signals
    cin = getbit   input 0
    x   = getbin 4 input 1
    y   = getbin 4 input 2
-- The circuit to be simulated
    (cout,s) = add4 cin x y
-- Format the output
    output =
      [string "  x =", bindec 3 x,
       string "  y =", bindec 3 y,
       string "  cin = ", bit cin,
       string "    ==>    cout = ", bit cout,
       string "  s =", bindec 3 s]
