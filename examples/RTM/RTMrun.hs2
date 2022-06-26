-- Simulation driver for RTM circuit

module Main where
import HDL.Hydra.Core.Lib
import RTM

----------------------------------------------------------------------------

-- sim_rtm n k inputdata
--   n = word size
--   k = address size, so there are 2^k registers

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
--        Inputs                   Effect            Expected Outputs
--   ld add d sa sb   x                              a      b     s    y
-- ~~~~~~~~~~~~~~~~~~~~      ~~~~~~~~~~~~~~~~~   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  [ "1  0  3  0  0  125"  -- R3 :=   x   = 125   R0=  0 R0=  0     0  125 (x)
  , "1  0  6  3  0   10"  -- R6 :=   x   =  10   R3=125 R0=  0   125   10 (x)
  , "1  1  2  3  6    0"  -- R2 := R3+R6 = 135   R3=125 R6= 10   135  135 (s)
  , "1  0  1  1  2   75"  -- R1 :=   x   =  75   R1=  0 R2=135   135   75 (x)
  , "1  1  1  1  2    0"  -- R1 := R1+R2 = 210   R1= 75 R2=135   210  210 (s)
  , "0  0  0  6  1    0"  -- nop                 R6= 10 R1=210   220    0 (x)
  ]

----------------------------------------------------------------------------
-- Simulation driver

sim_rtm n k input = driver $ do
--Input data
  useData rtm_testinput_1

-- Input ports      
  in_ld  <- inPortBit "ld"
  in_add <- inPortBit "add"
  in_d   <- inPortWord "d"  k
  in_sa  <- inPortWord "sa" k
  in_sb  <- inPortWord "sb" k
  in_x   <- inPortWord "x"  n

-- Input signals
  let ld  = inbsig in_ld
  let add = inbsig in_add
  let d   = inwsig in_d
  let sa  = inwsig in_sa
  let sb  = inwsig in_sb
  let x   = inwsig in_x

-- Circuit
  let (a,b,y,c,s) = rtm n k ld add d sa sb x
  
-- Format the signals  
  format      
    [string "Input: ",
     bit ld, string " ", bit add, bindec 2 d, bindec 2 sa, bindec 2 sb,
     bindec 4 x,
     string "  Output: ",
     bindec 4 a, bindec 4 b, bindec 4 s, bindec 4 y]

-- Run the simulation
  runSimulation
