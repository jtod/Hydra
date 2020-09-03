module TestCirc where

import Hydra

-- inports and logic gates, no feedback

mkMD :: String -> MDstring
mkMD xs = MDstring xs
mkTag :: String -> TagString
mkTag xs = TagString xs

type S = Component Bool MDstring TagString

----------------------------------------------------------------------
-- test1: 
--   a = inport
--   b = inport
--   c = inv a
--   d = dff b

test1 = do
  putStrLn "test1"

  putStrLn "building a = inport"
  let a = inportState defaultReader (mkMD "inport_a") (mkTag "a")
            :: S
  (nodeA, tagsA) <- dfs [] a
  nodeAstr <- showNode nodeA
  putStrLn ("nodeA = " ++ nodeAstr)
  putStrLn ("tagsA = " ++ showTags tagsA)

  putStrLn "building b = inport"
  let b = inportState defaultReader (mkMD "inport_b") (mkTag "b")
            :: S
  (nodeB, tagsB) <- dfs tagsA b
  nodeBstr <- showNode nodeB
  putStrLn ("nodeB = " ++ nodeBstr)
  putStrLn ("tagsB = " ++ showTags tagsB)

  putStrLn "building c = inv a"
  let c = inv a
            :: S
  (nodeC, tagsC) <- dfs tagsB c
  nodeCstr <- showNode nodeC
  putStrLn ("nodeC = " ++ nodeCstr)
  putStrLn ("tagsC = " ++ showTags tagsC)

  putStrLn "building d = dff b"
  let d = dfftag (stringToTag "dff_d") b
            :: S
  (nodeD, tagsD) <- dfs tagsC d
  nodeDstr <- showNode nodeD
  putStrLn ("nodeD = " ++ nodeDstr)
  putStrLn ("tagsD = " ++ showTags tagsD)

  putStrLn "starting looper"
  looper1 nodeA nodeB nodeC nodeD 0
  
looper1 nodeA nodeB nodeC nodeD cycle
  | cycle >= 5 = return ()
  | otherwise = do
      putStrLn ("\nCycle " ++ show cycle)
      a <- settle nodeA
      b <- settle nodeB
      c <- settle nodeC
      d <- settle nodeD
      putStrLn ("  a = " ++ showSig a
                  ++ ",  b = " ++ showSig b
                  ++ ",  c = " ++ showSig c
                  ++ ",  d = " ++ showSig d
               )
      advance nodeA
      advance nodeB
      advance nodeC
      advance nodeD
      looper1 nodeA nodeB nodeC nodeD (cycle+1)


----------------------------------------------------------------------
-- test2 is reg1

--   ld = inport
--   x = inport
--   r = dff (or2 (and2 (inv ld) r)
--                (and2 ld x))

test2 = do
  putStrLn "test2 is reg1 circuit"
  let ld = inportState defaultReader (mkMD "inport_ld") (mkTag "ld")
            :: S
  let x = inportState defaultReader (mkMD "inport_x") (mkTag "x")
            :: S
  let r = dff "r" (or2 (and2 (inv ld) r)
                       (and2 ld x))

  (nodeld, tagsld) <- dfs [] ld
  (nodex, tagsx) <- dfs tagsld x
  (noder, tagsr) <- dfs tagsx r
  putStrLn ("tagsr = " ++ showTags tagsr)
  putStrLn "starting looper"
  looper2 nodeld nodex noder 0
  
looper2 nodeld nodex noder cycle
  | cycle >= 20 = return ()
  | otherwise = do
      putStrLn ("\nCycle " ++ show cycle)
      ld <- settle nodeld
      x <- settle nodex
      r <- settle noder
      putStrLn ("  ld = " ++ showSig ld
                  ++ ",  x = " ++ showSig x
                  ++ ",  r = " ++ showSig r
               )
      advance nodeld
      advance nodex
      advance noder
      looper2 nodeld nodex noder (cycle+1)
