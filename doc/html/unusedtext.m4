
# Additional notes (n

Implementation
![rtm42](figures/png/rtm42-screenshot.png "rtm42 schematic diagram")




**Exercise.**
Use Hydra to simulate the *and2* and *xor3* logic
  gates, and print their truth tables.

**Exercise.**
Use Hydra to print out a truth table for *halfAdd*.
  Hint: do the carry and sum separately, and define a ``wrapper''
  circuit whose type matches the type needed for truthTable31, which
  is *Signal a => a -> a -> a -> a*.

**Exercise.**
Define a simulation driver (test bench) for *rippleAdd8*,
  define some suitable input data, and run the simulation.  Check that
  the outputs are correct.

**Exercise.**
We have two word signals *x* and *y*, which have the same
  word size (but we don't know or care exactly what the size is).
  There is a single control bit *c*.  Define a signal *z* which is
  a word; on each clock cycle, *z* is the same as *x* if *c=0*
  during that cycle, but *z* is the same as *y* if *c=1*.

**Exercise.**
Study the *BSR4* example circuit and its test bench (download
  these from the Moodle page).  Work out your predicted outputs
  from the circuit, given the input values that are provided.  Then
  run the simulation, and compare the simulated outputs with your
  predictions.

**Exercise.**
Suppose *x = [x0,x1,x2]*, *y = [y0,y1,y2,y3]*, and
  *z = x++y*.  What are the values of *z*,
  *length z*, and *z!!4*?

**Solution.**
~~~~~
z = [x0, x1, x2, y0, y1, y2, y3]
length z = 7
z !! 4 = y1
~~~~~

**Exercise.** A circuit has the type declaration *circ :: Signal a =>
a -> (a,a) -> [a] -> (a,[a])*.  How many groups of input bits are
there?  How are they structured?  How is the output structured?

**Solution.** There are three arrows in the type, so there are three
groups of input bits.  The first is a singleton bit; the second is a
pair of bits; the third is a word of bits.  The output is a pair
consisting of a singleton bit and a word of bits.

**Exercise.** Modify the definition of *rippleAdd4* to handle 6-bit
words.

**Solution.** This can be done manually by following the pattern used
in *rippleAdd4*:

~~~~~
rippleAdd6 :: Signal a => a -> [(a,a)] -> (a,[a])
rippleAdd6 cin [(x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)]
  = (c0, [s0,s1,s2,s3,s4,s5])
  where
    (c0,s0) = fullAdd (x0,y0) c1
    (c1,s1) = fullAdd (x1,y1) c2
    (c2,s2) = fullAdd (x2,y2) c3
    (c3,s3) = fullAdd (x3,y3) c4
    (c4,s4) = fullAdd (x4,y4) c5
    (c5,s5) = fullAdd (x5,y5) cin
~~~~~

Of course, a more elegant way is simply to use the general
*rippleAdd*, which works for all word sizes, including 6-bit words.
There is little point in defining special cases like *rippleAdd6*.

**Exercise.** Define an 8-bit adder, named *rippleAdd8*.  Don't follow
the pattern of *rippleAdd4*, with eight equations.  Instead, use
*rippleAdd4* as a building block circuit.  In your definition of
*rippleAdd8*, use two separate internal *rippleAdd4* circuits, and
connect them up appropriately.

**Solution.** As noted above, we could just define *rippleAdd8 =
rippleAdd*, or extend the lengthy manual definition to contain 8
equations.  However, this problem is not actually pointless: in some
technologies (e.g. the SSI chip family), you may have a *rippleAdd4*
primitive, and you really want the solution to contain just two chips.
The point of this problem is handling the carries properly.

~~~~~
rippleAdd8 cin [(x0,y0),(x1,y1),(x2,y2),(x3,y3),
                (x4,y4),(x5,y5),(x6,y6),(x7,y7)]
  = (cout, [s0,s1,s2,s3,s4,s5,s6,s7]
  where  (cout, [s0,s1,s2,s3])
            = rippleAdd4 cc [(x0,y0),(x1,y1),(x2,y2),(x3,y3)]
         (c, [s4,s5,s6,s7])
            = rippleAdd4 cin [(x4,y4),(x5,y5),(x6,y6),(x7,y7)]
~~~~~

This solution is still unnecessarily long, as it mentions all the
signals explicitly.  It could be shortened a bit, using some of
Hydra's operators on words, but if you have to design using SSI or MSI
components you will be stuck with tedious designs that don't scale
well.  Usually you're better off with building block circuits (*reg1*,
*mux1*, *fullAdd*, and the rest) along with design patterns.

**Exercise.** Use Hydra to similate the *and2* and *xor3* logic gates,
and print their truth tables.

**Solution.**

~~~~~
$ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :load TestAdd4
[1 of 2] Compiling Add4             ( Add4.hs, interpreted )
[2 of 2] Compiling Main             ( TestAdd4.hs, interpreted )
Ok, modules loaded: Main, Add4.
*Main> truthTable21 and2
Loading package transformers-0.3.0.0 ... linking ... done.
Loading package array-0.4.0.0 ... linking ... done.
Loading package deepseq-1.3.0.0 ... linking ... done.
Loading package containers-0.4.2.1 ... linking ... done.
Loading package pretty-1.1.1.0 ... linking ... done.
Loading package template-haskell ... linking ... done.
Loading package HydraLib-0.9.1 ... linking ... done.
    0 0 | 0
    0 1 | 0
    1 0 | 0
    1 1 | 1
*Main> truthTable31 xor3
    0 0 0 | 0
    0 0 1 | 1
    0 1 0 | 1
    0 1 1 | 0
    1 0 0 | 1
    1 0 1 | 0
    1 1 0 | 0
    1 1 1 | 1
*Main>
~~~~~

**Exercise.** Use Hydra to print out a truth table for *halfAdd*.
Hint: do the carry and sum separately, and define a ``wrapper''
circuit whose type matches the type needed for truthTable31, which is
*Signal a => a -> a -> a -> a*.

**Solution.**
Define wrapper circuits like this.

~~~~~
module Main where

-- Wrapper for halfAdd and fullAdd
-- Allows the tools for generating truth tables can be used.

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

wrapha_c x y  = cout
  where (cout,s) = halfAdd x y
wrapha_s x y = s
  where (cout,s) = halfAdd x y

wrapfa_c x y z  = cout
  where (cout,s) = fullAdd (x,y) z
wrapfa_s x y z  = s
  where (cout,s) = fullAdd (x,y) z
~~~~~

Now you can generate the truth tables for the wrapper circuits.

~~~~~
$ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :load Wrap
[1 of 1] Compiling Main             ( Wrap.hs, interpreted )
Ok, modules loaded: Main.
*Main> truthTable21 wrapha_c
Loading package transformers-0.3.0.0 ... linking ... done.
Loading package array-0.4.0.0 ... linking ... done.
Loading package deepseq-1.3.0.0 ... linking ... done.
Loading package containers-0.4.2.1 ... linking ... done.
Loading package pretty-1.1.1.0 ... linking ... done.
Loading package template-haskell ... linking ... done.
Loading package HydraLib-0.9.1 ... linking ... done.
    0 0 | 0
    0 1 | 0
    1 0 | 0
    1 1 | 1
*Main> truthTable21 wrapha_s
    0 0 | 0
    0 1 | 1
    1 0 | 1
    1 1 | 0
*Main> truthTable31 wrapfa_c
    0 0 0 | 0
    0 0 1 | 0
    0 1 0 | 0
    0 1 1 | 1
    1 0 0 | 0
    1 0 1 | 1
    1 1 0 | 1
    1 1 1 | 1
*Main> truthTable31 wrapfa_s
    0 0 0 | 0
    0 0 1 | 1
    0 1 0 | 1
    0 1 1 | 0
    1 0 0 | 1
    1 0 1 | 0
    1 1 0 | 0
    1 1 1 | 1
*Main>
~~~~~

**Exercise.** Define a simulation driver (test bench) for
*rippleAdd8*, define some suitable input data, and run the simulation.
Check that the outputs are correct.

**Solution.**
First, here is a module defining several adders at specific word
sizes.  The module is named Add4, but it also defines adders for 6
and 8 bit words.

~~~~~
module Add4 where

-- Define some adders at specific word sizes, manually specifying each
-- signal and component (a better approach, of course, is to use a
-- design pattern).

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

-- A 4-bit adder, constructed explicitly from 4 full adders.

rippleAdd4 :: Signal a => a -> [(a,a)] -> (a,[a])
rippleAdd4 cin [(x0,y0),(x1,y1),(x2,y2),(x3,y3)]
  = (c0, [s0,s1,s2,s3])
  where
    (c0,s0) = fullAdd (x0,y0) c1
    (c1,s1) = fullAdd (x1,y1) c2
    (c2,s2) = fullAdd (x2,y2) c3
    (c3,s3) = fullAdd (x3,y3) cin

-- A 6-bit adder, extending rippleAdd4 in the obvious way.

rippleAdd6 :: Signal a => a -> [(a,a)] -> (a,[a])
rippleAdd6 cin [(x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)]
  = (c0, [s0,s1,s2,s3,s4,s5])
  where
    (c0,s0) = fullAdd (x0,y0) c1
    (c1,s1) = fullAdd (x1,y1) c2
    (c2,s2) = fullAdd (x2,y2) c3
    (c3,s3) = fullAdd (x3,y3) c4
    (c4,s4) = fullAdd (x4,y4) c5
    (c5,s5) = fullAdd (x5,y5) cin

-- An 8-bit adder constructed from two 4-bit adders.  The signals are
-- named explicitly.

rippleAdd8 cin [(x0,y0),(x1,y1),(x2,y2),(x3,y3),
                (x4,y4),(x5,y5),(x6,y6),(x7,y7)]
  = (cout, [s0,s1,s2,s3,s4,s5,s6,s7])
  where (cout, [s0,s1,s2,s3])
           = rippleAdd4 cc [(x0,y0),(x1,y1),(x2,y2),(x3,y3)]
        (cc, [s4,s5,s6,s7])
           = rippleAdd4 cin [(x4,y4),(x5,y5),(x6,y6),(x7,y7)]
~~~~~

A test bench, RunAdd.hs:

~~~~~
module Main where

-- Run various binary adder circuits, including several that are
-- defined at specific word sizes (4, 6, 8 bits) and the general k-bit
-- ripple carry adder instanciated at 16 bits.

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import Add4

-- Test data

test_data_add4 =
--  c  x  y
  [[0,  5,  8],
   [1,  2, 12]]

test_data_add6 =
--  c  x  y
  [[0,  5,  8],
   [1,  2, 12],
   [0, 41, 13]]

test_data_add8 =
--  c    x    y
  [[0,   5,   8],
   [1,   2,  12],
   [0,  41,  13],
   [0, 103,  59],
   [0, 178, 193],
   [1,  17, 209]]

test_data_add16 =
--  c      x      y
  [[0,     5,     8],
   [1,     2,    12],
   [0,    41,    13],
   [0,   103,    59],
   [0,   178,   193],
   [0,  9037, 20185],
   [0, 31000, 32000],
   [0, 51000, 40000],
   [1,    17,   209]]

------------------------------------------------------------------------
-- main program

separator :: IO ()
separator = putStrLn (take 72 (repeat '-'))

main :: IO ()
main =
  do separator
     putStrLn "4-bit adder"
     run_adder rippleAdd4 4 test_data_add4

     separator
     putStrLn "6-bit adder"
     run_adder rippleAdd6 6 test_data_add6

     separator
     putStrLn "8-bit adder"
     run_adder rippleAdd8 8 test_data_add8

     separator
     putStrLn "16-bit adder"
     run_adder rippleAdd 16 test_data_add16

     separator

------------------------------------------------------------------------
-- Test bench

type Bit = Stream Bool
type Word = [Bit]

run_adder
  :: (Bit -> [(Bit,Bit)] -> (Bit,Word))
  -> Int
  -> [[Int]]
  -> IO ()

run_adder adder k input = runAllInput input output
  where
    cin = getbit   input 0
    x   = getbin k input 1
    y   = getbin k input 2

    (cout,s) = adder cin (zip x y)

    output =
      [string "  x = ", bindec 6 x,
       string "  y = ", bindec 6 y,
       string "  cin = ", bit cin,
       string "     ==>     cout = ", bit cout,
       string "  s = ", bindec 6 s]
~~~~~

Output from running main in RunAdd:

~~~~~
*Main>  main
Loading package transformers-0.2.2.0 ... linking ... done.
Loading package syb ... linking ... done.
Loading package array-0.2.0.0 ... linking ... done.
Loading package containers-0.2.0.1 ... linking ... done.
Loading package packedstring-0.1.0.1 ... linking ... done.
Loading package pretty-1.0.1.0 ... linking ... done.
Loading package template-haskell ... linking ... done.
Loading package HydraLib-0.7.2 ... linking ... done.
------------------------------------------------------------------------
4-bit adder
  x =      5  y =      8  cin = 0     ==>     cout = 0  s =     13
  x =      2  y =     12  cin = 1     ==>     cout = 0  s =     15
------------------------------------------------------------------------
6-bit adder
  x =      5  y =      8  cin = 0     ==>     cout = 0  s =     13
  x =      2  y =     12  cin = 1     ==>     cout = 0  s =     15
  x =     41  y =     13  cin = 0     ==>     cout = 0  s =     54
------------------------------------------------------------------------
8-bit adder
  x =      5  y =      8  cin = 0     ==>     cout = 0  s =     13
  x =      2  y =     12  cin = 1     ==>     cout = 0  s =     15
  x =     41  y =     13  cin = 0     ==>     cout = 0  s =     54
  x =    103  y =     59  cin = 0     ==>     cout = 0  s =    162
  x =    178  y =    193  cin = 0     ==>     cout = 1  s =    115
  x =     17  y =    209  cin = 1     ==>     cout = 0  s =    227
------------------------------------------------------------------------
16-bit adder
  x =      5  y =      8  cin = 0     ==>     cout = 0  s =     13
  x =      2  y =     12  cin = 1     ==>     cout = 0  s =     15
  x =     41  y =     13  cin = 0     ==>     cout = 0  s =     54
  x =    103  y =     59  cin = 0     ==>     cout = 0  s =    162
  x =    178  y =    193  cin = 0     ==>     cout = 0  s =    371
  x =   9037  y =  20185  cin = 0     ==>     cout = 0  s =  29222
  x =  31000  y =  32000  cin = 0     ==>     cout = 0  s =  63000
  x =  51000  y =  40000  cin = 0     ==>     cout = 1  s =  25464
  x =     17  y =    209  cin = 1     ==>     cout = 0  s =    227
------------------------------------------------------------------------
*Main> 
~~~~~


**Exercise.** Study the *BSR4* example circuit and its test bench
(download these from the Moodle page).  Work out your predicted
outputs from the circuit, given the input values that are provided.
Then run the simulation, and compare the simulated outputs with your
predictions.

**Solution.** To simulate a sequential circuit, build a simulation
table with a row for each clock cycle, and a column for each signal.
The top row is for cycle 0, and you should initialize all the flip
flop output signals to 0 (or to ?).  For each cycle, fill in the
values of the input signal columns.  Then work out the combinational
signals by simulating the logic gates.  When you're finished with
that, the entire row will be filled in.

Once a row has been completely filled in, you have simulated the
signals reaching validity during a clock cycle.

The cycle ends with a clock tick.  To simulate the clock tick, copy
the values of the signals that are input to a flip flop in the
current row to the corresponding flip flop output signal in the row
below.  Do this for all the flip flops.  Also, put the values of
the circuit's input signals into the corresponding columns of the
new row.

Keep going, a row at a time, until you want to stop the simulation.

The essential point of this problem is to get the timing right.
During a clock cycle the logic gates are calculating the inputs to
the registers; those inputs won't actually enter the register until
the clock tick (which is in the future) and you won't be able to
observe the new state of the register until the next clock cycle
(which is even farther into the future).  For example, if a shift
register contains 1000 and you have an opcode that says to shift to
the right with a left input of 1, the *current* register
readout is 1000, but on the *next* cycle the register will
output 1100.  A common error is to think that the future
event---the execution of the shift operation---has already
happened.
