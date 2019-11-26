module Count4 where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational


-- Two versions of a 4 bit counter are defined.  The first version
-- (count4a) uses explicit dff and mux1 circuits for each bit
-- position.  The second version (count4b) simplifies the full circuit
-- by using a buildinb block (cbit).

-- Version (a): explicit dff, mux1, halfAdd in each bit position

count4a :: CBit a => a -> [a]
count4a reset = [x0,x1,x2,x3]
  where
    x0 = dff (mux1 reset y0 zero)
    x1 = dff (mux1 reset y1 zero)
    x2 = dff (mux1 reset y2 zero)
    x3 = dff (mux1 reset y3 zero)
    (c0,y0) = halfAdd x0 c1
    (c1,y1) = halfAdd x0 c2
    (c2,y2) = halfAdd x2 c3
    (c3,y3) = halfAdd x3 one

-- Version (b): user building block cbit

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

