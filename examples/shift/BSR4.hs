module BSR4 where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

sr4 :: CBit a => (a,a) -> a -> a -> [a] -> (a,a,[a])
sr4 op li ri [x0,x1,x2,x3] = (y0, y3, [y0,y1,y2,y3])
  where
     y0 = srb op li y1 x0
     y1 = srb op y0 y2 x1
     y2 = srb op y1 y3 x2
     y3 = srb op y2 ri x3

srb :: CBit a => (a,a) -> a -> a -> a -> a
srb op li ri x = y
  where y = dff (mux2 op y x li ri)
