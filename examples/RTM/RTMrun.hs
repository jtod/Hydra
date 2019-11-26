-- Simulation driver for RTM circuit

module RTMrun where
import HDL.Hydra.Core.Lib
import RTM

----------------------------------------------------------------------------
-- Main program: run simulation

main =
  do putStrLn "Register Transfer Machine"
     sim_rtm 8 3 rtm_testinput_1

----------------------------------------------------------------------------
-- Test data

-- The effect is:
--   if ld=0 then no state change
--   if ld=1 then reg[d] := if add=0 then x else s

-- The expected outputs are:
--   a = R[sa]
--   b = R[sb]
--   s = a+b  (output of adder; carry output is ignored)
--   y = if add=0 then x else s  (data bus; value to be loaded if ld=1)

-- The only "real" outputs of the circuit are a and b.  The internal
-- signals s and y are also printed to see how the circuit works.


rtm_testinput_1 =
--       Inputs                   Effect            Expected Outputs
--  ld add d sa sb   x                              a      b     s    y
-- ~~~~~~~~~~~~~~~~~~~      ~~~~~~~~~~~~~~~~~   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  [[1, 0, 3, 0, 0, 125], -- R3 :=   x   = 125   R0=  0 R0=  0     0  125 (x)
   [1, 0, 6, 3, 0,  10], -- R6 :=   x   =  10   R3=125 R0=  0   125   10 (x)
   [1, 1, 2, 3, 6,   0], -- R2 := R3+R6 = 135   R3=125 R6= 10   135  135 (s)
   [1, 0, 1, 1, 2,  75], -- R1 :=   x   =  75   R1=  0 R2=135   135   75 (x)
   [1, 1, 1, 1, 2,   0], -- R1 := R1+R2 = 210   R1= 75 R2=135   210  210 (s)
   [0, 0, 0, 6, 1,   0]] -- nop                 R6= 10 R1=210   220    0 (x)

----------------------------------------------------------------------------
-- Simulation driver

sim_rtm n k input = runAllInput input output
  where
    ld  = getbit   input 0
    add = getbit   input 1
    d   = getbin k input 2
    sa  = getbin k input 3
    sb  = getbin k input 4
    x   = getbin n input 5
    (a,b,y,c,s) = rtm n k ld add d sa sb x
    output =
      [string "Input: ",
       bit ld, bit add, bindec 2 d, bindec 2 sa, bindec 2 sb,
       bindec 4 x,
       string "  Output: ",
       bindec 4 a, bindec 4 b, bindec 4 s, bindec 4 y]
