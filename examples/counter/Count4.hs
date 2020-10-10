-- Count4: a 4-bit binary counter circuit
-- This file is part of Hydra; see Hydra/README.md for copyright and license

module Count4 where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

-- Two versions of a 4 bit counter are defined.  The first version
-- (count4a) uses explicit dff and mux1 circuits for each bit
-- position.  The second version (count4b) simplifies the full circuit
-- by using a building block circuit (cbit).

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

