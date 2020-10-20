-- Count4: a 4-bit binary counter circuit
-- This file is part of Hydra; see Hydra/README.md for copyright and license

module Count4 where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

-- Several versions of a 4 bit counter are defined.  The first version
-- (count4a) uses explicit dff and mux1 circuits for each bit
-- position.  The second version (count4b) simplifies the full circuit
-- by using a building block circuit (cbit).  The third and fourth
-- versions use a ripple carry adder to do the addition. The final
-- examples show how to count up by an arbitary constant.

-- Version (a): explicit dff, mux1, halfAdd in each bit position.  This
-- definition is straightforward but not concise.

count4a :: CBit a => a -> [a]
count4a reset = [x0,x1,x2,x3]
  where
    x0 = dff (mux1 reset y0 zero)
    x1 = dff (mux1 reset y1 zero)
    x2 = dff (mux1 reset y2 zero)
    x3 = dff (mux1 reset y3 zero)
    (c0,y0) = halfAdd x0 c1
    (c1,y1) = halfAdd x1 c2
    (c2,y2) = halfAdd x2 c3
    (c3,y3) = halfAdd x3 one

-- Version (b): user building block cbit.  Notice that for each bit
-- position there is a similar circuit, containing a dff and a mux1.
-- Let's define that as a circuit, and call it cbit (for counter bit).
-- The definition of cbit is simple, and it simplifies count4b.  This
-- definition is better style than count4a.

count4b :: CBit a => a -> [a]
count4b reset = [x0,x1,x2,x3]
  where (c0,x0) = cbit reset c1
        (c1,x1) = cbit reset c2
        (c2,x2) = cbit reset c3
        (c3,x3) = cbit reset one

cbit :: CBit a => a -> a -> (a,a)
cbit reset cin = (cout,s)
  where
    s = dff (mux1 reset s' zero)
    (cout,s') = halfAdd cin s

-- count4c is similar to count4a, but it uses a ripple carry adder
-- instead of separate half adders.  This approach uses sets the carry
-- input to the ripple adder to one, and it just adds 0000 to x.
-- Since the circuit uses a ripple carry adder, we have a full adder
-- in every bit position.  The full adder is more general than
-- necessary (the previous circuits just use a half adder).

count4c :: CBit a => a -> [a]
count4c reset = [x0,x1,x2,x3]
  where
    x0 = dff (mux1 reset y0 zero)
    x1 = dff (mux1 reset y1 zero)
    x2 = dff (mux1 reset y2 zero)
    x3 = dff (mux1 reset y3 zero)
    (cout, [y0,y1,y2,y3]) = rippleAdd one [(x0,zero),(x1,zero),(x2,zero),(x3,zero)]

-- count4d is essentially the same as count4c, but it sets the carry
-- input to 0 and it adds the word 0001 to x.

count4d :: CBit a => a -> [a]
count4d reset = [x0,x1,x2,x3]
  where
    x0 = dff (mux1 reset y0 zero)
    x1 = dff (mux1 reset y1 zero)
    x2 = dff (mux1 reset y2 zero)
    x3 = dff (mux1 reset y3 zero)
    (cout, [y0,y1,y2,y3]) = rippleAdd zero [(x0,zero),(x1,zero),(x2,zero),(x3,one)]

-- Just for fun, try setting the carry input to 1 and also add 0001.
-- This will increment the counter by 2 each cycle.  So count4e will
-- count 0, 2, 4, ...

count4by2 :: CBit a => a -> [a]
count4by2 reset = [x0,x1,x2,x3]
  where
    x0 = dff (mux1 reset y0 zero)
    x1 = dff (mux1 reset y1 zero)
    x2 = dff (mux1 reset y2 zero)
    x3 = dff (mux1 reset y3 zero)
    (cout, [y0,y1,y2,y3]) = rippleAdd one [(x0,zero),(x1,zero),(x2,zero),(x3,one)]

-- What if we want to increment by something other than 1?  The
-- solution is to add the desired constant each cycle.  This approach
-- lets you count up by any constant.  The carry input isn't useful
-- for that, and half adders can't do it.  This circuit illustrates
-- the technique by adding 3 every cycle (3 = 0011).

count4by3 :: CBit a => a -> [a]
count4by3 reset = [x0,x1,x2,x3]
  where
    x0 = dff (mux1 reset y0 zero)
    x1 = dff (mux1 reset y1 zero)
    x2 = dff (mux1 reset y2 zero)
    x3 = dff (mux1 reset y3 zero)
    (cout, [y0,y1,y2,y3]) = rippleAdd zero [(x0,zero),(x1,zero),(x2,one),(x3,one)]
