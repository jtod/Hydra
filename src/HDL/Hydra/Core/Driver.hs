-- Driver: simulation drivers for Hydra circuits
-- This file is part of Hydra.  See README, https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module       : HDL.Hydra.Circuits.SimDriver
Description  : Tools for writing simulation drivers
Copyright    : (c) 2022 John T. O'Donnell
License      : GPL-3
Maintainer   : john.t.odonnell9@gmail.com
Stability    : experimental

Tools for writing simulation drivers for circuits specified in Hydra -}

-- This module supplies a wide variety of data formatting services,
-- including conversions between number systems, converting input
-- strings into internal signal representations, and converting
-- signals to readable strings for output.

module HDL.Hydra.Core.Driver
 (
-- * re-export so user doesn't hvae to import Control.Monad.State
   StateT (..), execStateT, liftIO,
   B,
-- * Configuration
   hydraVersion, printVersion,
-- * New from step driver
   call, noContinuation, bitsBin,
--   logFileName,
   clockTick,
   conditional, cmdLogging,
   tryRead, maybeRead, mkFullPath,
   intToBits,
--    putLogStr, putLogStrLn, DEPRECATED
   putBufLogStr, putBufLogStrLn, flushBufLog, writeLogStr,
   setStateWsIO,
   incrementCycleCount, clearCycleCount,
   bitsHex, setFlagTable, checkFlag, advanceFlagTable, getClockCycle,
   runFormat, getUserState, printError, setHalted, setPeek, advancePeeks,
   takeInputsFromList, observeSignals, advanceSignals,
--   InputLists, selectInputList, storeInputList, getInputList,  testInputLists,
   Command, defineCommand, Operation,
   defineStandardCommands,
   printLine,
   parseBit, parseBits,
   Driver,  driver,  --  DriverMain,
   SysState (..), InPort (..), OutPort (..),
   initState,
   useData,
   inPortBit, inputBit,
   inPortWord, inputWord,
   outputBit, outputWord,
   cycleCmd,
   readAllInputs, writeBufs, readInput,
   showBSig, printInPorts, printInPort,
   printOutPorts, printOutPort,
   advanceInPorts, advanceOutPorts,
   runSimulation,
   
-- * Number system conversions
  intsInt, ints16hex4,

-- * Extracting input signals from input data
-- | The "input side" of a testbench takes inputs provided by the user
-- | and converts them to the signals required as actual inputs to the
-- | circuit.  The user inputs can be written as decimal numbers, and
-- | the functions defined below perform number system conversions
-- | (e.g. converting a decimal number to a word of bits), as well as
-- | generating the internal data structures used to represent
-- | signals.

  getbit, getbit2, getbin, biti, gettc,

-- * Constructing output format
  Format (..), format, fmtWordsGeneral,
--  mskipfmts,
  FormatMode (..),
  advanceFormat,
  clockcycle,
  run, runUntil, runAllInput,

-- * State of testbench
  simstate, setStateWs, fmtIf,

-- * Generating output text from output signals

-- | How to take the output signals from a circuit and format the text
-- | to be printed.

  string, bit, binhex, bindec, tcdec, hex, hexbin, bitstc, bits

 )

where

import HDL.Hydra.Core.Signal
import HDL.Hydra.Core.SigStream
import HDL.Hydra.Core.SigBool
import HDL.Hydra.Core.PrimData

import qualified Data.Map as Map
import Control.Monad.State
import Control.Concurrent
import Control.Exception
import Data.IORef
import Data.Maybe
import Data.List
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe
-- import System.Console.ANSI

---------------------------------------------------------------------------
-- Configuration
---------------------------------------------------------------------------

hydraVersion :: String
hydraVersion = "3.6.0e"  -- Automate this in make SetVersion

defaultLogFileName :: String
defaultLogFileName = "logCircuit.txt"

printVersion :: IO ()
printVersion = putStrLn ("Hydra version " ++ hydraVersion)
---------------------------------------------------------------------------
-- Core simulation driver tools
---------------------------------------------------------------------------

conditional :: Bool -> StateT (SysState a) IO ()
  -> StateT (SysState a) IO ()
conditional b op =
  case b of
    True -> do op
               return ()
    False -> return ()


---------------------------------------------------------------------------
-- Default signal representation
---------------------------------------------------------------------------

type B = Stream Bool
type CB = Stream Bool

---------------------------------------------------------------------------
-- Driver
---------------------------------------------------------------------------

type Driver a = IO (SysState a)

driver ::  (StateT (SysState a) IO ()) ->IO ()
driver f = do
  printVersion
  execStateT f initState >> return ()

---------------------------------------------------------------------------
-- System state
---------------------------------------------------------------------------

-- Simulation mode.  The default is Interactive, but if the driver
-- performs useData the mode will be changed to Batch.

data Mode = Batch | Interactive deriving (Eq, Read, Show)

noContinuation :: StateT (SysState b) IO ()
noContinuation = do
--  lift $ putStrLn "noContinuation"
  return ()

-- Type b is the optional user driver state; it is used to extend the
-- basic system state with additional fields. Thus the user state has
-- type Maybe b.  (obsolete anything that can refer to the user state
-- incorporates a)

data SysState b = SysState
  { running :: Bool
  , mode :: Mode
  , halted :: Bool
  , writingLogFile :: Bool
  , logFileName :: String
  , bufferedLog :: [String]
  , formatMode :: FormatMode
  , commands :: Commands b
  , cycleCount :: Int
  , continueOp :: StateT (SysState b) IO ()
  , writeBuffer :: [Int]
  , cycleCountSinceClear :: Int
  , currentInputString :: String
  , inPortList :: [InPort]
  , nInPorts :: Int
  , outPortList :: [OutPort]
  , formatSpec :: Maybe [Format Bool b]
  , peekList :: [[Stream Bool]]
  , flagTable :: [(String, Stream Bool)]
  , breakpointKey :: String
  , storedInput :: [String]
  , selectedKey :: String
  , userState :: Maybe b
  }

initState :: SysState a
initState = SysState
  { running = True
  , mode = Interactive
  , halted = False
  , bufferedLog = []
  , writingLogFile = False
  , logFileName = defaultLogFileName
  , formatMode = FormatNormal
  , commands = Map.empty
  , cycleCount = 0
  , continueOp = noContinuation
  , writeBuffer = []
  , cycleCountSinceClear = 0
  , currentInputString = ""
  , inPortList = []
  , nInPorts = 0
  , outPortList = []
  , formatSpec = Nothing
  , peekList = []
  , flagTable = []
  , breakpointKey = ""
  , storedInput = []
  , selectedKey = ""
  , userState = Nothing
  }

getClockCycle :: StateT (SysState a) IO Int
getClockCycle = do
  s <- get
  return (cycleCount s)

incrementCycleCount :: StateT (SysState a) IO ()
incrementCycleCount = do
  s <- get
  let i = cycleCount s
  let j = cycleCountSinceClear s
  put $ s {cycleCount = i+1, cycleCountSinceClear  = j+1}

clearCycleCount :: StateT (SysState a) IO ()
clearCycleCount = do
  s <- get
  put $ s {cycleCountSinceClear = 0}

setHalted :: Format Bool a
setHalted = FmtSetHalted

----------------------------------------------------------------------
-- User driver state
----------------------------------------------------------------------

-- The simulation driver maintains its own state, consisting of a pair
-- (c,s) where c :: Int is the clock cycle number, and s ::
-- DriverState contains information the driver saves for its own use
-- later on.

getUserState :: StateT (SysState a) IO (Maybe a)
getUserState = do
  s <- get
  return (userState s)

putUserState :: a -> StateT (SysState a) IO ()
putUserState x = do
  s <- get
  put (s {userState = Just x})
  return ()

printError :: String -> StateT (SysState a) IO ()
printError msg = do
  liftIO $ putStrLn ("System error: " ++ msg)
  return ()

---------------------------------------------------------------------------
-- Interactive command loop
---------------------------------------------------------------------------

commandLooper :: StateT (SysState a) IO ()
commandLooper = do
  s <- get
--  liftIO $ putStrLn ("cmdlooper" ++ show (running s))
  case running s of
    False -> return ()
    True -> do
      doCommand
      commandLooper

runCmd :: [String] -> StateT (SysState a) IO ()
runCmd _ = runLooper

runLooper :: StateT (SysState a) IO ()
runLooper = do
  s <- get
  case running s of
    False -> return ()
    True -> do
      cycleCmd []
      runLooper

doCommand :: StateT (SysState a) IO ()
doCommand = do
  liftIO $ putStr "hydra> "
  liftIO $ hFlush stdout
  xs <- liftIO $ hGetLine stdin
  let ws = words xs
--  printLine ("doCommand: ws = " ++ show ws)
  case ws of
    [] -> do
      cycleCmd []
    (x:_) -> do
--      printLine ("doCommand c = " ++ c)
      mcmd <- lookupCommand x
      case mcmd of
        Nothing -> do
          printLine ("Invalid command: " ++ x ++ "enter h for help")
          doCommand
        Just (name,cmd,help) -> do
          cmd ws

------------------------------------------------------------------------
-- Interactive commands
------------------------------------------------------------------------

-- A commannd takes a list of arguments and then runs in the StateT
-- monad.  Many commands don't use the arguments.  In general, the
-- first element of the argument list should be the name of the
-- command, and subsequent elements provide options or data to the
-- command.

-- Command is establishInputs -> args -> operation

-- Operation with user state type a and returning type b
type Operation a = StateT (SysState a) IO ()

type Command a = [String] -> Operation a
type Commands a = Map.Map String (CommandSpec a)
type CommandSpec a = (String, Command a, String)

defineCommand :: String -> Command a -> String -> StateT (SysState a) IO ()
defineCommand name cmd help = do
  s <- get
  let cmds' = Map.insert name (name,cmd,help) (commands s)
  put (s {commands = cmds'})

lookupCommand :: String -> StateT (SysState a) IO (Maybe (CommandSpec a))
lookupCommand key = do
  s <- get
  return $ Map.lookup key (commands s)

-- Default commands.  The simulation driver can define additional
-- commands, or override these default ones.

defineStandardCommands :: StateT (SysState a) IO ()
defineStandardCommands = do
  defineCommand "cycle"  cycleCmd        "perform one clock cycle"
--  defineCommand "run"    runCmd          "repeat clock cycles"
--  defineCommand "r"      runCmd          "repeat clock cycles"
  defineCommand "help"   helpCmd         "display list of commands"
  defineCommand "h"      helpCmd         "display list of commands"
  defineCommand "quit"   quitCmd         "terminate the simulation"
  defineCommand "q"      quitCmd         "terminate the simulation"
--  defineCommand "select" selInpCmd       "select ARG as current input data"
--  defineCommand "inputs" showInListsCmd  "display the input lists"


cmdLogging :: String -> StateT (SysState b) IO ()
cmdLogging flag = do
  if flag == "on"
    then do s <- get
            put s {writingLogFile = True}
  else if flag == "off"
    then do s <- get
            put s {writingLogFile = False}
  else lift $ putStrLn "Usage: logging on or logging off"
  return ()

helpCmd :: [String] -> StateT (SysState a) IO ()
helpCmd _ = displayHelpMessage

displayHelpMessage :: StateT (SysState a) IO ()
displayHelpMessage = do
  s <- get
  let cmds = commands s
  mapM_ showCmd cmds

setStringLength :: Int -> String -> String
setStringLength w xs = xs ++ take (w - length xs) (repeat ' ')

showCmd :: CommandSpec a -> StateT (SysState a) IO ()
showCmd (name,cmd,help) = do
  liftIO $ putStrLn (setStringLength 8 name ++ "   " ++ help)

quitCmd :: Command a
quitCmd args = do
  s <- get
  put $ s {running = False}
  return ()

---------------------------------------------------------------------------
-- Breakpoint
---------------------------------------------------------------------------

setFlagTable :: [(String, Stream Bool)] -> StateT (SysState a) IO ()
setFlagTable flagTable = do
  s <- get
  put $ s {flagTable}

checkFlag :: [(String, Stream Bool)] -> String -> Bool
checkFlag table key =
  let elt = find (\(a,b) -> a == key) table
  in case elt of
       Nothing -> False
       Just (_, x) -> is1 (current x)

advanceFlagTable :: StateT (SysState a) IO ()
advanceFlagTable = do
  s <- get
  let t = flagTable s
  let t' = map advanceFlag t
  put $ s {flagTable = t'}

advanceFlag :: (String, Stream Bool) -> (String, Stream Bool)
advanceFlag (key, x) = (key, future x)

---------------------------------------------------------------------------
-- Ports
---------------------------------------------------------------------------

-- An input port must be defined for each input, because it holds the
-- buffer in which the input data is placed.  The system maintains a
-- list of all InPorts in the state.

data InPort
  = InPortBit
      { inPortName :: String
      , inbsig  :: Stream Bool
      , inbuf :: IORef Bool
      , fetcher :: [IO Bool]
      , inPortIndex :: Int
--      , inBitRead :: String -> Maybe Bool
      }
  | InPortWord
      { inPortName :: String
      , inwsig  :: [Stream Bool]
      , inwbufs :: [IORef Bool]
      , wfetchers :: [[IO Bool]]
      , inwsize :: Int
      , inPortIndex :: Int
      }

data OutPort      
  = OutPortBit
      { outPortName :: String
      , outbsig  :: CB
      }
  | OutPortWord
      { outPortName :: String
      , outwsig  :: [Stream Bool]
      , outwsize :: Int
--      , showw :: [Bool] -> String
      }

-- make a new inport bit

inPortBit :: String -> StateT (SysState a) IO InPort
inPortBit inPortName = do
  inbuf <- liftIO $ newIORef False
  let fetcher = repeat (readIORef inbuf)
  let inbsig = listeffects fetcher
  s <- get
  let inPortIndex = nInPorts s
--printLine ("inPortBit " ++ inPortName ++ " " ++ show inPortIndex)
  let port = InPortBit { inPortName, inbsig, inbuf, fetcher, inPortIndex }
  put $ s {inPortList = inPortList s ++ [port], nInPorts = inPortIndex + 1}
  return port

inputBit :: String -> StateT (SysState a) IO (Stream Bool)
inputBit inPortName = do
  inbuf <- liftIO $ newIORef False
  let fetcher = repeat (readIORef inbuf)
  let inbsig = listeffects fetcher
  s <- get
  let inPortIndex = nInPorts s
--printLine ("inputBit " ++ inPortName ++ " " ++ show inPortIndex)
  let port = InPortBit { inPortName, inbsig, inbuf, fetcher, inPortIndex }
  put $ s {inPortList = inPortList s ++ [port], nInPorts = inPortIndex + 1}
  return inbsig
-- return (inbsig port)
--  let outsig = inbsig port
--  return outsig
--  return port

-- make a new inport word

inPortWord :: String -> Int -> StateT (SysState a) IO InPort
inPortWord inPortName inwsize = do
  inwbufs <- liftIO $ mkInBufWord inwsize
  let wfetchers = map (\x -> repeat (readIORef x)) inwbufs
  let inwsig = map listeffects wfetchers
  s <- get
  let inPortIndex = nInPorts s
  let port = InPortWord { inPortName, inwsig, inwbufs, wfetchers,
                          inwsize, inPortIndex }
  put $ s {inPortList = inPortList s ++ [port], nInPorts = inPortIndex + 1}
--  return inwsig
  return port
--  liftIO $ putStrLn ("inPortWord len inwbufs = " ++ show (length inwbufs))
--  liftIO $ putStrLn ("inPortWord len wfetchers = " ++ show (length wfetchers))
--  liftIO $ putStrLn ("inPortWord len inwsig = " ++ show (length inwsig))

inputWord :: String -> Int -> StateT (SysState a) IO [Stream Bool]
inputWord inPortName inwsize = do
  inwbufs <- liftIO $ mkInBufWord inwsize
  let wfetchers = map (\x -> repeat (readIORef x)) inwbufs
  let inwsig = map listeffects wfetchers
  s <- get
  let inPortIndex = nInPorts s
  let port = InPortWord { inPortName, inwsig, inwbufs, wfetchers,
                          inwsize, inPortIndex }
  put $ s {inPortList = inPortList s ++ [port], nInPorts = inPortIndex + 1}
  return inwsig
--  return port

mkInBufWord :: Int -> IO [IORef Bool]
mkInBufWord n
  | n == 0 = return []
  | n > 0  = do x <- newIORef False
                xs <- mkInBufWord (n-1)
                return (x:xs)

writeBufs :: [IORef Bool] -> [Bool] -> IO ()
writeBufs rs bs = mapM_ (\(r,b) -> writeIORef r b ) (zip rs bs)

-- make a new outport bit

outputBit :: String -> CB -> StateT (SysState a) IO OutPort
outputBit outPortName outbsig = do
  s <- get
  let port =  OutPortBit { outPortName, outbsig }
  s <- get
  put $ s {outPortList = outPortList s ++ [port]}
  return port

-- make a new outport word

-- outputWord :: String -> [CB] -> ([Bool]->String) -> StateT (SysState a) IO OutPort
-- outputWord outPortName outwsig showw = do
outputWord :: String -> [CB] -> StateT (SysState a) IO OutPort
outputWord outPortName outwsig = do
  s <- get
  let outwsize = length outwsig
--  let port =  OutPortWord { outPortName, outwsig, outwsize, showw }
  let port =  OutPortWord { outPortName, outwsig, outwsize }
  put $ s {outPortList = outPortList s ++ [port]}
  return port

---------------------------------------------------------------------------
-- Clock cycle
---------------------------------------------------------------------------

-- A clock cycle proceeds in a sequence of phases:

-- Phase 1: Establish inputs
-- Phase 2: Observe signals
-- Phase 3: Advance signals

{-
doClockCycle :: StateT (SysState DriverState) IO ()
doClockCycle = do
  s <- get
  let i = cycleCount s
  establishInputs ...  (takeInputsFromList xs)
-- m1ClockCycle
-- runM1simulation
  conditional displayFullSignals $ do
    printInPorts
    printOutPorts
  runFormat FormatNormal
  advanceInPorts
  advanceOutPorts
  advancePeeks -- advanceExtra
  advanceFlagTable  -- advanceExtra
  advanceFormat
  incrementCycleCount

-}

cycleCmd :: [String] -> StateT (SysState a) IO ()
cycleCmd args = do
--  printLine "clockCycle..."
  s <- get
  let i = cycleCount s
  let inp = inPortList s
  let outp = outPortList s
  liftIO $ putStrLn (take 70 (repeat '-'))
  liftIO $ putStrLn ("Cycle " ++ show i)
  inputsFromListOrInteractive
  s <- get
  case running s of
    False -> return ()
    True -> do
      let mfmt = formatSpec s
      case mfmt of
        Nothing -> do
    --      printLine ("cycleCmd mfmt = Nothing, printing ports")
          printInPorts
          printOutPorts
        Just fs -> do
    --      printLine ("cycleCmd mfmt = fs, running format")
--          runFormat True True
          runFormat FormatNormal
      clockTick
--      advanceInPorts
--      advanceOutPorts
--      advancePeeks
--      advanceFlagTable
--      advanceFormat
--      incrementCycleCount
--      s <- get
--      put (s {cycleCount = i+1})

clockTick :: StateT (SysState b) IO ()
clockTick = do
  advanceInPorts
  advanceOutPorts
  advancePeeks
  advanceFlagTable
  advanceFormat
  incrementCycleCount


---------------------------------------------------------------------------
-- Phase 1: Establish inputs
---------------------------------------------------------------------------

-- Decide what the input signal values should be for this clock cycle
-- and store them into the inport buffers.

inputsFromListOrInteractive :: StateT (SysState a) IO ()
inputsFromListOrInteractive = do
  s <- get
  case mode s of
    Batch -> do
      mx <- getStoredInput
      case mx of
        Nothing -> do
          s <- get
          put $ s {running = False}
        Just x -> do
          takeInputsFromList x
      return ()

    Interactive -> do
      readAllInputs
      return ()

{-
inputsFromListOrInteractive :: StateT (SysState a) IO ()
inputsFromListOrInteractive = do
  mx <- getStoredInput
  case mx of
    Nothing -> readAllInputs
    Just x -> do
--      printLine ("Taking inputs from stored data: " ++ x)
      takeInputsFromList x
-}

--------------------------------------------------
-- Interactive input
--------------------------------------------------

readAllInputs :: StateT (SysState a) IO ()
readAllInputs = do
  s <- get
  let ports = inPortList s
  mapM_ readInput ports
  return ()

readInput :: InPort -> StateT (SysState a) IO ()
readInput (InPortBit {..}) = do
  x <- liftIO $ interactiveReadBit inPortName
  liftIO $ writeIORef inbuf x
readInput (InPortWord {..}) = do
  xs <- liftIO $ interactiveReadWord inPortName inwsize
  liftIO $ writeBufs inwbufs xs
  return ()

-- Given a port name, prompt the user to enter a string of bits
-- (e.g. 100101), confert to [Bool] and return

-- Should drop whitespace and validate input
interactiveReadBit :: String -> IO Bool
interactiveReadBit portname = do
--  putStrLn "interactiveReadBit"
  putStrLn ("Enter value of " ++ portname)
  ys <- getLine
  let ws = words ys
  let x = read (ws!!0) :: Int
  let b = if x==1 then True else False
  return b
--  return (if head ys == '1' then True else False)

readSigBool :: String -> IO Bool
readSigBool xs = do
  putStrLn ("Enter value of " ++ xs ++ " (either 0 or 1, then enter)")
  putStrLn xs
  ys <- getLine
  return (if head ys == '1' then True else False)

interactiveReadWord :: String -> Int -> IO [Bool]
interactiveReadWord portname nbits = do
  putStrLn "interactiveReadWord"
  putStrLn ("   Enter " ++ portname
            ++ " (number from 0 through " ++ show (2^nbits - 1) ++ ")")
  xs <-getLine
  let r = words xs !! 0
  let val = read r :: Int
  let bs = intToBits val nbits
--  putStrLn ("interReadW n=" ++ show nbits ++ "bs=" ++ show bs)
  return bs
--  case parseBits xs n of
--    Just r -> return r
--    Nothing -> interactiveReadWord portname n

-- Quick version of a function to read a signal value.  Requires first
-- char to be either '1' or '0'.  Later, should parse the input
-- properly

readSigString :: String -> IO String
readSigString xs = do
  putStrLn xs
  ys <- getLine
  putStrLn ("readSigString = <" ++ ys ++ ">")
  return ys

--------------------------------------------------
-- Obtain input data from stored input list
--------------------------------------------------

-- An input list is a list of strings; each string can be parsed to
-- obtain the values of input signals for a clock cycle.

useData :: [String] -> StateT (SysState a) IO ()
useData storedInput = do
  s <- get
  put (s {storedInput, mode = Batch})
  
getStoredInput :: StateT (SysState a) IO (Maybe String)
getStoredInput = do
  s <- get
  case storedInput s of
    [] -> do
      put $ s {running = False}
      return Nothing
    (x:xs) -> do
      put $ s {storedInput = xs}
      return (Just x)

-- This is an older version...

-- The inputs are parsed from SysState.currentInputString and stored
-- into the signal buffers.  It is essential to ensure that all input
-- signals have been set.  After this is completed, all signal values
-- are fixed for the duration of the clock cycle.

takeInputsFromList :: String -> StateT (SysState a) IO ()
takeInputsFromList line = do
  s <- get
  let ports = inPortList s
  let xs = words line
  mapM_ takeInput (zip ports xs)
  return ()

-- The input must be a number

takeInput :: (InPort, String) -> StateT (SysState a) IO ()
takeInput (p,x) = do
  s <- get
  case p of
    InPortBit {..} -> do
      let y = read x :: Int
      let b = y==1
      liftIO $ writeIORef inbuf b
    InPortWord {..} -> do
      let y = read x :: Int
      let bs = intToBits y inwsize
      liftIO $ writeBufs inwbufs bs

---------------------------------------------------------------------------
-- Phase 3: Observe and print signals
---------------------------------------------------------------------------

observeSignals :: StateT (SysState a) IO ()
observeSignals = do
  printInPorts
  printOutPorts

showBSig :: Bool -> String
showBSig False  = "0"
showBSig True = "1"

printInPorts :: StateT (SysState a) IO ()
printInPorts = do
  s <- get
  let ports = inPortList s
  let i = cycleCount s
  liftIO $ putStrLn ("Input signals during cycle " ++ show i)
  mapM_ printInPort ports

printInPort :: InPort -> StateT (SysState a) IO ()
printInPort (InPortBit {..}) = do
  let x = current inbsig
  let xs = "    " ++ inPortName ++ " = " ++ showBSig x
  liftIO $ putStrLn xs
printInPort (InPortWord {..}) = do
  let xs = bitsBin (map current inwsig)
  let ys = "    " ++ inPortName ++ " = " ++ show xs
  liftIO $ putStrLn ys

printOutPorts :: StateT (SysState a) IO ()
printOutPorts = do
  s <- get
  let ports = outPortList s
  let i = cycleCount s
  if length ports > 0
    then liftIO $ putStrLn ("Output signals during cycle " ++ show i)
    else return ()
  mapM_ printOutPort ports

printOutPort :: OutPort -> StateT (SysState a) IO ()
printOutPort (OutPortBit {..}) = do
  let x = current outbsig
  let xs = "    " ++ outPortName ++ " = " ++ showBSig x
  liftIO $ putStrLn xs
printOutPort (OutPortWord {..}) = do
--  let xs = showw (map current outwsig)
  let xs = bitsBin (map current outwsig)
  let ys = "    " ++ outPortName ++ " = " ++ show xs
  liftIO $ putStrLn ys

opCurrentPorts :: StateT (SysState a) IO ()
opCurrentPorts = do
  printInPorts
  printOutPorts

---------------------------------------------------------------------------
-- Phase 4: Advance signal steppers
---------------------------------------------------------------------------

advanceSignals :: StateT (SysState a) IO ()
advanceSignals = do
  advanceInPorts
  advanceOutPorts

advance :: StateT (SysState a) IO ()
advance = do
  advanceInPorts
  advanceOutPorts
  advancePeeks
  advanceFlagTable

advancePeeks :: StateT (SysState a) IO ()
advancePeeks = do
  s <- get
  let ps = peekList s
  let ps' = map advancePeek ps
  put $ s {peekList = ps'}

advancePeek :: [Stream Bool] -> [Stream Bool]
advancePeek xs = map future xs

advanceInPorts :: StateT (SysState a) IO ()
advanceInPorts = do
  s <- get
  let ports = inPortList s
  ports' <- mapM advanceInPort ports
  put $ s {inPortList = ports'}

advanceInPort :: InPort -> StateT (SysState a) IO InPort
advanceInPort (InPortBit {..}) = do
  return $ InPortBit {inbsig = future inbsig, ..}
advanceInPort (InPortWord {..}) = do
  let xs  = map future inwsig
  return $ InPortWord {inwsig = xs, ..}

advanceOutPorts :: StateT (SysState a) IO ()
advanceOutPorts = do
  s <- get
  let ports = outPortList s
  ports' <- mapM advanceOutPort ports
  put $ s {outPortList = ports'}

advanceOutPort :: OutPort -> StateT (SysState a) IO OutPort
advanceOutPort (OutPortBit {..}) =
  return $ OutPortBit {outPortName, outbsig = future outbsig}
advanceOutPort (OutPortWord {..}) = do
  let xs = map future outwsig
--  return $ OutPortWord {outPortName, outwsig=xs, outwsize, showw}
  return $ OutPortWord {outPortName, outwsig=xs, outwsize}

---------------------------------------------------------------------------
--		       Simulation Driver Input
---------------------------------------------------------------------------


---------------------------------------------------------------------------
--		       Simulation Driver Output
---------------------------------------------------------------------------

setPeek :: [Stream Bool] -> StateT (SysState a) IO ()
setPeek xs = do
  s <- get
  let ps = peekList s
  put $ s {peekList = ps ++ [xs]}

---------------------------------------------------------------------------
-- Output format specification
---------------------------------------------------------------------------

-- The output section of a simulation driver uses format specifiers to
-- control how various signals are printed.  These format specifiers
-- are string, bit, bindec, binhex, tcdec.  The base signal type is a
-- (e.g. Bool), and b is the driver state type

data Format a b
  = FmtContinue (StateT (SysState b) IO ())
  | FmtState (b -> String)
  | FmtSetStateWords (b->[[a]]->b) [[Stream a]]
  | FmtSetStateWordsIO ([[a]] -> StateT (SysState b) IO ()) [[Stream a]]
  | FmtIf (Stream a) [Format a b] [Format a b]
  | FmtString String
  | FmtBit (String -> Stream a -> (String, Stream a))
      String (Stream a)
  | FmtWord
      (String -> Int -> Int -> [Stream a]
         -> (String, [Stream a]))
      String Int Int [Stream a]
  | FmtHex (Int -> [Stream a] -> (String, [Stream a])) Int [Stream a]
  | FmtWordsGeneral ([[Int]]->String) [[Stream a]]

  | FmtCycle (Int -> String) -- deprecated
  | FmtSetHalted -- deprecated
--  | FmtCase [Stream a] [(Int, Format a b)] -- not used?

---------------------------------------------------------------------------
-- User functions for constructing format specifications
---------------------------------------------------------------------------

call :: StateT (SysState b) IO () -> Format a b
call op = FmtContinue op

bit x = FmtBit fmtBit [] x
bits x = FmtWord fmtWord [] n n x
  where n = length x

-- | string xs is a format specifier that will put the literal string
-- | xs into the output.  This is useful for inserting labels into the
-- | output.

string :: String -> Format a b
string x = FmtString x

simstate f = FmtState f

clockcycle f = FmtCycle f
setStateWs f xs = FmtSetStateWords f xs
setStateWsIO f xs = FmtSetStateWordsIO f xs

bindec w x = FmtWord fmtBin [] w (length x) x

binhex x = FmtHex fmtHex (length x) x  -- deprecated, use hex instead
hex x = FmtHex fmtHex (length x) x
bitstc w x = FmtWord fmtTC  [] w (length x) x  -- deprecated, use tcdec

tcdec w x = FmtWord fmtTC  [] w (length x) x

fmtWordsGeneral f xs = FmtWordsGeneral f xs

----------------------------------------
-- Convert current cycle value to string
----------------------------------------

-- The fmtBit function converts a bit signal for output.  The argument
-- x is a bit signal, and a character is output each clock cycle
-- giving the value of the signal.

-- ?? todo remove label

fmtBit :: (Signal a, Static a) => String -> Stream a -> (String, Stream a)
fmtBit lbl x = (showSig (current x), future x)

-- A word signal (a list of bit signals) can be formatted for output
-- in a variety of ways.  To display it as a sequence of bits, specify
-- the format with fmtWord.

fmtWord
 :: (Signal a, Static a)
  => String -> Int -> Int -> [Stream a] -> (String, [Stream a])
fmtWord lbl w k x =
  let z = setlength w (concat (map (showSig . current) x))
  in (z, map future x)

-- A word signal can be converted to a decimal integer for output.  If
-- the word is binary, use the fmtBin function, but if it's two's
-- complement, use fmtTC.

fmtBin, fmtTC
  :: (Signal a, Static a)
  => String -> Int -> Int -> [Stream a] -> (String, [Stream a])

fmtBin lbl w k x =
  let z = map (sigInt . current) x
      n = sum [b * 2^(k-(i+1)) | (b,i) <- zip z [0..]]
  in (setlength w (show n), map future x)

fmtTC lbl w k x =
  let z = map (sigInt . current) x
      n = sum [b * 2^(k-(i+1)) | (b,i) <- zip z [0..]]
      m = if head z == 0
          then n
          else n - 2^k
  in (setlength w (show m), map future x)

-- A sequential word signal x can be converted to k-digit hexadecimal
-- using fmtHex k x.

fmtHex :: Static a => Int -> [Stream a] -> (String, [Stream a])
fmtHex k x =
  let z = map (sigInt . current) x
      j = k `mod` 4
      z' = take j (repeat 0) ++ z
      s = [hexDig (field z' i 4) | i <- [0,4..k-1]]
  in (s, map future x)

fmtIf x fs1 fs0 = FmtIf x fs1 fs0


format :: [Format Bool b] -> StateT (SysState b) IO ()
format xs = do
  s <- get
  put (s {formatSpec = Just xs})

-- if a then carry out action.  if p then produce output.  In any
-- case, advance the format

---------------------------------------------------------------------------
-- Run the format
---------------------------------------------------------------------------

-- Run through the format and perform specified actions, including
-- performing output.  The signals are not changed; that must be done
-- separately by performing advanceFormat.

-- use separated execute/advance operations on format

runFormat :: FormatMode -> StateT (SysState a) IO ()
runFormat m = do
--  lift $ putStrLn ("****** runFormat " ++ show m)
  s <- get
  let mfmt = formatSpec s
  case mfmt of
    Nothing -> return ()
    Just fs -> doFmts m fs
  flushBufLog

-- Configure whether format action will produce output

data FormatMode
  = FormatNormal      -- check formatMode for configured output
  | FormatFull        -- produce all specified output including I/O
  | FormatQuiet       -- full output on normal cycle, quiet for I/O
  deriving (Eq, Show)

condMode :: FormatMode -> StateT (SysState a) IO () -> StateT (SysState a) IO ()
condMode m action =
  case m of
    FormatNormal -> action
    FormatFull -> action
    FormatQuiet -> return ()

-- Run the format using current values of signals, but the signals
-- are not advanced

doFmts :: StaticBit a => FormatMode -> [Format a b] -> StateT (SysState b) IO ()

doFmts m [] = return ()
doFmts m (x:xs) =
  do doFmt m x
     doFmts m xs

-- Process a format element but leave signals in current state

doFmt :: StaticBit a => FormatMode -> Format a b -> StateT (SysState b) IO ()

doFmt m (FmtContinue op) = do
--  lift $ putStrLn "doFmt FmtContinue about to set continueOo"
  s <- get
  put s {continueOp = op}

doFmt m (FmtState f) =
  do mds <- getUserState
     case mds of
       Nothing -> return ()
       Just ds -> do condMode m $ putBufLogStr (f ds)
                     return ()

doFmt m (FmtSetStateWords f xs) = do
  mds <- getUserState
  case mds of
    Nothing -> return ()
    Just ds -> do
      let ws = map (map current) xs
      let ds' = f ds ws
      putUserState ds'
      return ()

doFmt m (FmtSetStateWordsIO f xs) = do
  mds <- getUserState
  case mds of
    Nothing -> return ()
    Just ds -> do
      let ws = map (map current) xs
      f ws  -- ?????
      return ()
--             let ds' = f ds ws
--             putUserState ds'

doFmt m (FmtIf x fs_then fs_else) = do
  let b = is1 (current x)
  case b of
    True -> doFmts m fs_then
    False -> doFmts m fs_else

doFmt m (FmtString s) = do
  condMode m $ putBufLogStr s

doFmt m (FmtBit f lbl x) = do
  let (s,x') = f lbl x
  condMode m $ putBufLogStr s
  return ()

doFmt m (FmtWord f lbl w k x) = do
  let (s,x') = f lbl w k x
  condMode m $ putBufLogStr s
  return ()

doFmt m (FmtHex f k x) = do
  let (s,x') = f k x
  condMode m $ putBufLogStr s
  return ()

doFmt m (FmtWordsGeneral f xs) = do
  mds <- getUserState
  case mds of
    Nothing -> return ()
    Just ds -> do
      let ys = map (map (sigInt . current)) xs
      let s = f ys
      condMode m $ putBufLogStr s

doFmt m (FmtCycle f) = do
  s <- get
  let cycle = cycleCount s
  condMode m $ putBufLogStr (f cycle)
  return ()

doFmt m FmtSetHalted = do
  s <- get
  put (s {halted = True})
  return ()

{- deprecated
--  fs_then' <- mdofmts (a && b) (p && b) fs_then
--  fs_else' <- mdofmts (a && not b) (p && not b) fs_else
--  return (FmtIf (future x) fs_then' fs_else')
--     fs_then' <- (if b then mdofmts else mskipfmts) fs_then
--     fs_else' <- (if b then mskipfmts else mdofmts) fs_else
--     return (FmtIf (future x) fs_then' fs_else')

-- It will set halted regardless of p
mdofmt a p FmtSetHalted = do
  case a of
    False -> return ()
    True -> do
      s <- get
      put (s {halted = True})
  return FmtSetHalted
-}

---------------------------------------------------------------------------
-- Advance all signals that appear in the format
---------------------------------------------------------------------------

-- advanceFormat :: (Signal a, StaticBit a) => StateT (SysState b) IO ()
advanceFormat = do
  s <- get
  let mfmt = formatSpec s
  case mfmt of
    Nothing -> return ()
    Just fs -> do
      fs' <- getAdvFmts fs
      s <- get
      put $ s {formatSpec = Just fs'}
      return ()

-- Traverse a format list and return the list with all signals advanced

getAdvFmts :: [Format a b] -> StateT (SysState b) IO [Format a b]
getAdvFmts [] = return []
getAdvFmts (f:fs) =
  do f' <- getAdvFmt f
     fs' <- getAdvFmts fs
     return (f' : fs')

-- Take a format element and return it with its signals advanced

getAdvFmt :: Format a b -> StateT (SysState b) IO (Format a b)

getAdvFmt (FmtContinue op) = return (FmtContinue op)

getAdvFmt (FmtState f) = return (FmtState f)

getAdvFmt (FmtSetStateWords f xs) =
  return (FmtSetStateWords f (map (map future) xs))

getAdvFmt (FmtSetStateWordsIO f xs) =
  return (FmtSetStateWordsIO f (map (map future) xs))

getAdvFmt (FmtIf x fs_then fs_else) = do
  let x' = future x
  fs_then' <- getAdvFmts fs_then
  fs_else' <- getAdvFmts fs_else
  return (FmtIf x' fs_then' fs_else')
  
getAdvFmt (FmtString s) = return (FmtString s)

getAdvFmt (FmtBit f lbl x) = do
  let (s,x') = f lbl x
  return (FmtBit f lbl x')

getAdvFmt (FmtWord f lbl w k x) = do
  let (s,x') = f lbl w k x
  return (FmtWord f lbl w k x')

getAdvFmt (FmtHex f k x) = do
  let (s,x') = f k x
  return (FmtHex f k x')

getAdvFmt (FmtWordsGeneral f xs) =
  return (FmtWordsGeneral f (map (map future) xs))

getAdvFmt (FmtCycle f) = return (FmtCycle f)
getAdvFmt FmtSetHalted = return FmtSetHalted

---------------------------------------------------------------------------
-- Running the simulation
---------------------------------------------------------------------------

runSimulation :: StateT (SysState a) IO ()
runSimulation = do
--  liftIO $ putLogStrLn "gamma"
  defineStandardCommands
  s <- get
  let m = mode s
  liftIO $ putStrLn (show m ++ " mode")
  case m of
    Batch -> do
      runLooper
      return ()
    Interactive -> do
      printLine "Enter command after prompt, h for help"
      commandLooper
  

---------------------------------------------------------------------------
-- Monadic version of run: Produce output cycle by cycle
---------------------------------------------------------------------------

-- (i,st) is state; i is cycle number, st is user's defined state

-- | Run a simulation until the end of the list of inputs is reached.

run :: (Signal a, StaticBit a)
  => b -> [[Int]] -> [Format a b] -> IO ()
run initst inps outs =
  do _ <- execStateT (mrun' (length inps) 0 outs) (0,initst)
     return ()

mrun' :: (Signal a, StaticBit a) =>
  Int -> Int -> [Format a b] -> StateT (Int,b) IO ()
mrun' limit i xs =
  if i<limit
  then do xs' <- mstep i xs
          mrun' limit (i+1) xs'
  else return ()

-- | Run a simulation until a termination predicate is satisfied.

runUntil :: (Signal a, StaticBit a) =>
  b -> ((Int,b)->Bool) -> [[Int]] -> [Format a b] -> IO ()
runUntil initst p inps outs =
  do _ <- execStateT (mrunUntil p outs) (0,initst)
     return ()

-- Tools for using runUntil, especially with simple format

termpred :: Int -> (Int,a) -> Bool
termpred inputSize (c,s) = (c >= inputSize)

-- | Run the simulation as long as the input list contains more input
-- | data.

runAllInput :: [[Int]] -> [Format Bool ()] -> IO ()
runAllInput input format =
  runUntil () (termpred (length input)) input format


-- version of mrun' with a termination predicate, rather than simply
-- running for a fixed number of cycles

mrunUntil :: (Signal a, StaticBit a) =>
  ((Int,b)->Bool) -> [Format a b] -> StateT (Int,b) IO ()
mrunUntil p xs =
  do s <- get
     if p s
       then return ()
       else do xs' <- mstep 0 xs  -- 0 is dummy, get rid	
               mrunUntil p xs'


mstep :: (Signal a, StaticBit a) =>
  Int -> [Format a b] -> StateT (Int,b) IO [Format a b]
mstep = undefined
{-
mstep i xs =
  do -- lift $ putStr (setlength 4 (show i))
     -- lift $ putStr ".  "
     ys <- mdofmts xs
     lift $ putStr "\n"
     (cycle,s) <- get
     put (cycle+1,s)
     return ys
-}

---------------------------------------------------------------------------
--  Number System Conversions
---------------------------------------------------------------------------

bitsBin :: [Bool] -> Int
bitsBin bs = binint (map boolInt bs)

-- inthexok w i gives w-char hex for i
-- use inthexok 4 x

bitsHex :: Int -> [Bool] -> String
bitsHex w bs = inthexok w (bitsBin bs)

parseBit :: String -> Maybe Bool
parseBit x =
  if head x == '1'
  then Just True
  else if head x == '0'
       then Just False
       else Nothing

-- Should drop whitespace and validate input

parseBits :: String -> Int -> Maybe [Bool]
parseBits xs n =
  if length xs == n
    then Just (map (== '1') xs)
    else Nothing

intToBits :: Int -> Int -> [Bool]
intToBits x n = reverse (intToBitsf x n)

intToBitsf :: Int -> Int -> [Bool]
intToBitsf x n
  | n == 0 = []
  | n > 0 = (x `mod` 2 /= 0) : intToBitsf (x `div` 2) (n-1)
  
-- convert a list of bits (represented as Ints) to an Int,
-- using binary representation

intsInt :: [Int] -> Int
intsInt xs =
  sum [x * 2^(n-(i+1)) | (x,i) <- zip xs [0..]]
  where n = length xs

-- convert a list of 16 bits (represented as Ints) to a 4-character
-- hex string

ints16hex4 :: [Int] -> String
ints16hex4 xs =
  map (int4hex . intsInt)
   [field xs 0 4, field xs 4 4, field xs 8 4, field xs 12 4]

-- | Convert a hexadecimal number, represented as a string of hex
-- | digits, to an Int.


hexbin :: Int -> String -> Int
hexbin k cs =
  let k' = k `div` 4 
      i = sum [hexDigVal c * 16^(k'-(i+1)) | (c,i) <- zip cs [0..]]
  in i

hextc :: Int -> String -> Int
hextc k cs =
  let i = sum [hexDigVal c * 16^(k-(i+1)) | (c,i) <- zip cs [0..]]
  in if hexDigVal (cs!!0) >=8
    then i - 2^(4*k)
    else i

hexDigVal :: Char -> Int
hexDigVal '0' =  0
hexDigVal '1' =  1
hexDigVal '2' =  2
hexDigVal '3' =  3
hexDigVal '4' =  4
hexDigVal '5' =  5
hexDigVal '6' =  6
hexDigVal '7' =  7
hexDigVal '8' =  8
hexDigVal '9' =  9
hexDigVal 'a' = 10
hexDigVal 'b' = 11
hexDigVal 'c' = 12
hexDigVal 'd' = 13
hexDigVal 'e' = 14
hexDigVal 'f' = 15

int4hex :: Int -> Char
int4hex i = "0123456789abcdef" !! i

hexDig :: [Int] -> Char
hexDig bs =
  let i = sum [b * 2^(4-(i+1)) | (b,i) <- zip bs [0..]]
  in "0123456789abcdef" !! i

{- ?? todo - remove this
hexDig' :: [Int] -> String
hexDig' bs =
  let i = sum [b * 2^(4-(i+1)) | (b,i) <- zip bs [0..]]
  in  show i --"0123456789abcdef" !! i
-}

-- Let xs be a k-bit word whose binary interpretation is n.  Then tc k
-- n gives the two's complement representation of the word.  For
-- example,

--  tc 8 5 = 5    (because 0000 0101 is nonnegative)
--  tc 8 255 = -1 (because 1111 1111 is negative and represents -1)

tc :: Int -> Int -> Int
tc k n =
  if n >= 2^(k-1)
    then n - 2^k
    else n

{-
tc :: Integral a => Word16 -> a
tc x =
  let s = testBit x 15
      y = if s
            then complement x + 1
            else x
      z = fromIntegral y
  in z
-}

---------------------------------------------------------------------------
-- I/O for logging
---------------------------------------------------------------------------

putBufLogStr :: String -> StateT (SysState b) IO ()
putBufLogStr x = do
  s <- get
  let xs = bufferedLog s
  put s {bufferedLog = x:xs}

putBufLogStrLn xs = putBufLogStr (xs ++ "\n")

flushBufLog :: StateT (SysState b) IO ()
flushBufLog = do
  s <- get
  let fileName = logFileName s
  let xs = concat (reverse (bufferedLog s))
  put s {bufferedLog = []}
  lift $ putStr xs
  s <- get
  case writingLogFile s of
    False -> return ()
    True -> do lift $ appendFile fileName xs

writeLogStr :: String -> String -> IO ()
writeLogStr logFileName xs = do
  putStr xs
  writeFile logFileName xs

{-
-- Write a string to both stdout and a log file
putLogStr :: String -> String -> IO ()
putLogStr logFileName xs = do
  putStr xs
  appendFile logFileName xs

putLogStrLn :: String -> IO ()
putLogStrLn xs = putLogStr logFileName (xs ++ "\n")
--  let ys = xs ++ "\n"
--  putStr ys
--  appendFile logFileName ys
-}


---------------------------------------------------------------------------
-- Terminal I/O
---------------------------------------------------------------------------

-- Check whether there is input available.  If so do a blocking line
-- read, and when the entire line is available read and return it.  If
-- there is no input, return immediately

readIfReady :: IO (Maybe String)
readIfReady = do
  b <- hReady stdin
  case b of
    False -> return Nothing
    True -> do
      xs <- hGetLine stdin
      return (Just xs)

--  testTextColors
testTextColors :: IO ()
testTextColors = do
  putStrLn "testTextColors is disabled"
  return ()

{-  
  setSGR [SetColor Foreground Vivid Red]
--  setSGR [SetColor Background Vivid Blue]
  putStrLn "This text is red"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "This text appears in the default colors"
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "This text is blue"
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn "Back to the default colors"
-}

---------------------------------------------------------------------------
-- Utilities
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- Utilities
---------------------------------------------------------------------------

tryRead :: String -> IO (Either IOError String)
tryRead path = do
  x <- try (readFile path)
  return x

maybeRead :: String -> IO (Maybe String)
maybeRead path = do
  r <- tryRead path
  case r of
    Left e -> return Nothing
    Right x -> return (Just x)

mkFullPath :: String -> String -> String
mkFullPath prefix path =
  prefix ++ path ++ ".obj.txt"

-- Text field adjustment

setlength :: Int -> String -> String
setlength w xs =
  let n = length xs
  in take (w-n) (repeat ' ') ++ xs

center :: Int -> String -> String
center i s =
  let k = length s
      j = (i - length s) `div` 2
  in constring ' ' j ++ s ++ constring ' ' (i-k-j)

constring :: Char -> Int -> String
constring c i = take i (repeat c)

indent :: Int -> String
indent k = take (2*k) (repeat ' ')

separatorLine :: IO ()
separatorLine = putStrLn (take 60 (repeat '-'))

justs :: [Maybe x] -> [x]
justs [] = []
justs (Nothing : xs) = justs xs
justs (Just x : xs) = x : justs xs

-- Take a list of actions, perform them on demand, and return the result

listeffects :: [IO a] -> Stream a
listeffects = mapListStream unsafePerformIO

mapListStream :: (a->b) -> [a] -> Stream b
mapListStream f x
  = Cycle (f (head x))
          (mapListStream f (tail x))

doNtimes :: Int -> StateT (SysState a) IO () -> StateT (SysState a) IO ()
doNtimes i f
  | i == 0   = return ()
  | otherwise = do
      f
      doNtimes (i-1) f

printLine :: String -> StateT (SysState a) IO ()
printLine xs = liftIO (putStrLn xs)

---------------------------------------------------------------------------
-- Probably to be deprecated soon
-- old style getbit etc for input signals...
---------------------------------------------------------------------------

---------------------------------------------------------------------------
--		       Simulation Driver Input
---------------------------------------------------------------------------

-- The input section of a simulation driver is constructed using
-- getbit, getbin, gettc, and ...

------------------------------------------------------------------------
-- Extract input signals from integer lists
------------------------------------------------------------------------

-- | Convert an input number (which should be 0 or 1) into an input
-- | bit signal.

getbit :: (Signal a, Static a) => [[Int]] -> Int -> Stream a
getbit xs i =
  Cycle (intSig (head xs !! i)) (getbit (tail xs) i)

-- | Convert an input number (which should be between 0 and 3) to a
-- | pair of input bit signals.

getbit2
  :: (Signal a, StaticBit a)
  => [[Int]] -> Int -> (Stream a, Stream a)
getbit2 xs i = (getbini 2 0 xs i, getbini 2 1 xs i)

getbit20, getbit21
  :: (Signal a, StaticBit a)
  => [[Int]] -> Int -> Stream a
getbit20 xs i =
  Cycle (bit20 (head xs !! i)) (getbit20 (tail xs) i)
getbit21 xs i =
  Cycle (bit21 (head xs !! i)) (getbit20 (tail xs) i)

bit20, bit21 :: (Signal a, Static a) => Int -> a
bit20 x = intSig ((x `mod` 4) `div` 2)
bit21 x = intSig (x `div` 2)

-- Makes a stream consisting of the j'th bit of the k-bit binary
-- representation of the i'th column of the list xs.

getbini k j xs i =
  Cycle (biti k j (head xs !! i)) (getbini k j (tail xs) i)

-- Build a k-bit word (list of streams) which is the binary
-- representation of the i'th column of xs::[[Int]].

getbin :: (Signal a, StaticBit a)
  => Int -> [[Int]] -> Int -> [Stream a]
getbin k xs i = [getbini k j xs i | j <- [0..k-1]]

-- Build a k-bit word (list of streams) which is the two's complement
-- representation of the i'th column of xs::[[Int]].

gettc k xs i = [getbini k j (map (map (tc k)) xs) i | j <- [0..k-1]]

-- The biti function gives the i'th bit of the k-bit binary
-- representation of the integer x. The wordsize is k, i is the bit
-- index, x is the integer input Bits are numbered from the left
-- starting with 0, thus bit 0 is the most significant bit.

biti :: (Signal a, StaticBit a) => Int -> Int -> Int -> a
biti k i x = boolSig (odd (x `div` (2^(k-(i+1)))))


------------------------------------------------------------------------
-- Deprecated or deleted
------------------------------------------------------------------------

-- From old Driver.hs...
--------------------------------

-- Stream version of run, original approach, no longer in use.  This
-- won't work any more, because of the change from Format a to Format
-- a b, as well as the introduction of new Fmt constructors.

{-
run inps outs = run' (length inps) 0 outs

run' :: (Signal a, Static a) => Int -> Int -> [Format a] -> IO ()
run' limit i xs =
 if i<limit
 then do xs' <- step i xs
         run' limit (i+1) xs'
 else return ()

step :: (Signal a, Static a) => Int -> [Format a] -> IO [Format a]
step i xs =
  do putStr (setlength 4 (show i))
     putStr ".  "
     let ys = map dofmt xs
     let s = concat (map fst ys)
     putStr s
     putStr "\n"
     let xs' = map snd ys
     return xs'

dofmt :: (Signal a, Static a) => Format a -> (String, Format a)
dofmt (FmtBit f lbl x) =
  let (s,x') = f lbl x
  in (s, FmtBit f lbl x')
dofmt (FmtWord f lbl w k x) =
  let (s,x') = f lbl w k x
  in (s, FmtWord f lbl w k x')
dofmt (FmtHex f k x) =
  let (s,x') = f k x
  in (s, FmtHex f k x')
dofmt (FmtString s) = (s, FmtString s)

dofmt (FmtIf x fs1 fs0) =
  let (out1, fs1') = dofmts fs1
      (out0, fs0') = dofmts fs0
      out = case is1 (current x) of
              True -> out1
              False -> out0
  in (out, FmtIf (future x) fs1' fs0')

dofmt (FmtWordsGeneral f xs) =
  let ys = map (map (sigInt . current)) xs
      zs = map (map future) xs
      s = f ys
  in (s, FmtWordsGeneral f zs)

-- used for FmtIf...
dofmts :: (Signal a, Static a) => [Format a] -> (String, [Format a])
dofmts fs =
  let ys = map dofmt fs
      s = concat (map fst ys)
      fs' = map snd ys
  in (s,fs')

-} -- end of stream version of run


-- Generic simulation drivers
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- simdriver
{-
-- module HDL.Hydra.Core.StepDriver where


-- import HDL.Hydra.Core.PrimData
-- import HDL.Hydra.Core.Signal
-- import HDL.Hydra.Core.SigStream
-- import HDL.Hydra.Core.SigBool
-}
--------------------------------------------------------------------------------
-- Formatting simulation output
--------------------------------------------------------------------------------

{-
-- Not used... was going to use these in new StepDriver
data NewFmt a
  = NewFmtString String
  | NewFmtBit a
  | NewFmtBits [a]
-}
-- These used in old Driver, drop them?  Now using Control.Monad.State
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Class
--      , inWordRead :: String -> Int -> Maybe [Bool]
--      , readw :: (String -> Int -> IO [Bool])
-- FmtSetStateWords...
--  do (c,s) <- get
--     let ws = map (map current) xs
--     let s' = f (c,s) ws
--     put (c,s')
--     return (FmtSetStateWords f (map (map future) xs))
--  do let ys = map (map (sigInt . current)) xs
--     let zs = map (map future) xs
--     let s = f ys
--     lift $ putStr s
--     return (FmtWordsGeneral f zs)
--          printLine "executing command..."
--          printLine "command done..."
{-
Functions that can be used in driver
  storePreparedInputData i data
  selectPreparedInputData i
  selectInteractiveInputData
  useInputData              convert strings to numbers for input data
  generateInputData         arbitrary function from string to list of inputs
  selectInputInteractive
  inputsFromListOrInteractive
  currentFormat i  - do current actions for format i
  currentInPorts
  currentOutPorts
  advance

  clockCycle

  testInputLists
-}


--    "Type command character, then Enter\n"
--  ++ "help  h     help, display this message\n"
--  ++ "step  s     perform one clock cycle"
--  ++ "quit  q     return to ghci prompt\n"
--  ++ "Enter, with other character, is same as step\n"
{-
Interactive user  commands
  > step  (also just enter with blank line)    -- do one clock cycle
  empty/blank line                            -- same as > step
  > step i                                     -- do i clock cycles
  > run                                        -- repeat step until halt or ^C
  > while predicate                            -- repeat step while pred is True
  > until predicate                            -- repeat step while pred is False
  > quit                                       -- terminate simulation
  > foobar  command defined in the driver
-}
--  , inputLists :: InputLists
--  , inputLists = Map.empty


--  let ms = userState s
--  case ms of
--    Just us -> return us
--    Nothing -> do
--      printError "getUserState: userState undefined, using default"
--      return x

  
--   let y = fromJust x
--   return y
--  case userState s of
--    Nothing -> do
--      printLine "Error: userState not defined"
--      return default
--    Just ds -> return ds

--  There are three ways to do
-- this: interactive input, using stored input data, or generating the
-- inputs.   Different simulation drivers will take different
-- approaches to this.  A set of functions is defined here, and the
-- simulation driver can use any of them.

-- Interactive input

-- Usage: driver simply executes readInputsInteractively

-- readInputsInteractively: Automatically prompt user for each input
--   port, read the value entered, and store into port buffer.  The
--   prompts use the labels provided when the input port is created.
--   The input port must be either a bit signal or a word of bits
--   signal.  A generic function is used to convert the string
--   provided by the user into the Bool or [Bool] signal required.

-- Usage: driver 

-- Using input lists.

--   takeInputsFromString
--   readInputList  -- copies the input string into currentInputString

-- inputsFromListOrInteractive uses data from selected input list, if any, and
-- otherwise asks the user to enter it interactively

-- Generating the input

-- setCurrentInputString -- Store an arbitrary string into
--   currentInputString.  This is useful for generators.  For example,
--   a boot function can read an object code file and parse it, then
--   produce a string giving the input signals needed for the object
--   code, and save it using currentInputString.

  {-
inputsFromListOrInteractive :: StateT (SysState a) IO ()
inputsFromListOrInteractive = do
  d <- getInputList
  case d of
    Just xs -> do
      printLine ("Taking inputs from stored data: " ++ xs)
      takeInputsFromList xs
    Nothing -> do
      printLine "Reading inputs from user:"
      readAllInputs
-}

--  x <- liftIO $ readSigBool ("  enter " ++ inPortName)

  --  xs <- liftIO $ readw inPortName inwsize
--  liftIO $ putStrLn ("readInput xs = " ++ show xs)
--  readval <- liftIO $ readSigString ("  enter " ++ inPortName)

  -- There may be
-- several input lists; this enables a driver to provide operations
-- that require several clock cycles, for example dumping a register
-- file.  The input lists are maintained in a finite map, with a
-- string as the key, and the value is a list of strings.

-- https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html

{-
type InputLists = Map.Map String (Int, [String])

selInpCmd :: [String] -> Operation a
selInpCmd xs =
  if length xs >= 2 then selectInputList (xs!!1) else return ()
-}

{-
-- (i,xs) i is index of next
-- isInputListEmpty :: Operation Bool
isInputListEmpty = do
  s <- get
  let fm = inputLists s
  let key = selectedKey s
  return $ case Map.lookup key fm of
             Just (i,xs) -> i >= length xs
             Nothing -> True
-}

{-
selectInputList :: String -> Operation a
selectInputList selectedKey = do
  s <- get
  put $ s {selectedKey}
--  let selectedKey = if length args >= 2 then args !! 1 else ""
--  printLine ("selectInputList key=<" ++ selectedKey ++ ">")

-- Get next item from selected input list
-}

  
{-
useInput :: [String] -> StateT (SysState a) IO ()
useInput xs = do
  s <- get
  put $ s {storedInput = xs}
-}

{-
storeInputList :: String -> [String] -> StateT (SysState a) IO ()
storeInputList key xs = do
  s <- get
  let fm = inputLists s
  let fm' = Map.insert key (0,xs) fm
  put $ s {inputLists = fm'}
-}

{-
resetInputList :: String -> StateT (SysState a) IO ()
resetInputList key = do
  s <- get
  let fm = inputLists s
  case Map.lookup key fm of
    Just (i,xs) -> do
      let fm' = Map.insert key (0,xs) fm
      put $ s {inputLists = fm'}
    Nothing -> return ()
-}
{-
testInputLists :: Operation a
testInputLists = do
  storeInputList "abc" ["cats", "and", "dogs"]
  storeInputList "def" ["hot", "cold"]
  storeInputList "ghi" ["blue", "red", "green"]
  selectInputList "def"
  a <- getInputList
  b <- getInputList
  selectInputList "abc"
  c <- getInputList
  selectInputList "ghi"
  d <- getInputList
  selectInputList "abc"
  e <- getInputList
  f <- getInputList
  g <- getInputList
  h <- getInputList
  i <- getInputList
  j <- getInputList
  printLine ("a = " ++ show a)
  printLine ("b = " ++ show b)
  printLine ("c = " ++ show c)
  printLine ("d = " ++ show d)
  printLine ("e = " ++ show e)
  printLine ("f = " ++ show f)
  printLine ("g = " ++ show g)
  printLine ("h = " ++ show h)
  printLine ("i = " ++ show i)
  printLine ("j = " ++ show j)
-}

-- Int -> Int -> [Int] ... intbin k x
-- [Int] -> Int...  binint tcint

  {-
opCurrentFormat :: Int -> StateT (SysState a) IO ()
opCurrentFormat i = do
  s <- get
  let fmt = formatList s !! i
  return ()
-}

  
{-
data Cmd
  = Quit        -- q
  | Help        -- h pr ?
  | ListInput   -- l
  | ReadInput   -- r
  | Boot        -- b
  | StepCycle   -- c  or space
  | Go          -- g
  | NoCmd       -- other
  deriving (Eq, Read, Show)

parseCmd :: String -> Cmd
parseCmd "" = StepCycle
parseCmd ('q' : _) = Quit
parseCmd ('h' : _) = Help
parseCmd ('?' : _) = Help
parseCmd ('l' : _) = StepCycle
parseCmd ('r' : _) = ReadInput
parseCmd ('b' : _) = Boot
parseCmd ('c' : _) = StepCycle
parseCmd (' ' : _) = StepCycle
parseCmd ('g' : _) = ReadInput
parseCmd _ = NoCmd


getCmdOperand :: IO (Maybe String)
getCmdOperand = do
  putStrLn "getCmdOperand"
  args <- getArgs
  putStrLn ("args = " ++ show args)
  let operand = if length args >= 1 then Just (args!!0) else Nothing
  return operand

-- old version, deprecating
getCmd :: IO (CmdOp, String)
getCmd = do
  putStrLn "getCmd"
  args <- getArgs
  putStrLn ("args = " ++ show args)
  let operand = if length args >= 2 then args!!1 else ""
  return $
    (if length args == 0 then CmdOpEmpty
        else if args!!0 == "batch" then CmdOpBatch
        else if args!!0 == "cli" then CmdOpCLI
        else CmdOpEmpty,
     operand)
-}

{-

-- Deprecated???
data CmdOp
  = CmdOpEmpty
  | CmdOpBatch
  | CmdOpCLI
  deriving (Eq,Show)
-}

  
{-
doCommand :: StateT (SysState a) IO ()
doCommand = do
  liftIO $ putStr "$ "
  xs <- liftIO $ hGetLine stdin
  let c = parseCmd xs
  case c of
    Quit -> do
      liftIO $ putStrLn "Quitting"
      s <- get
      put $ s {running = False}
      return ()
    NoCmd -> return ()
    Help -> do liftIO $ putStrLn helpMessage
    StepCycle -> do
      clockCycle []
      return ()
    _ -> return ()
-}

-- ************************************************************************
-- old format version beginning

------------------------------------------------------------------------
{- old version calls mdofmts
runFormat :: Bool -> Bool -> StateT (SysState a) IO ()
runFormat a p = do
  s <- get
  let mfmt = formatSpec s
  case mfmt of
    Nothing -> return ()
    Just fs -> do
      fs' <- mdofmts a p fs
      s <- get
      put $ s {formatSpec = Just fs'}
--      liftIO $ putStrLn ""

-- Just advance the format but don't produce output
advanceFormat ::  StateT (SysState b) IO ()
advanceFormat = runFormat True False

advanceFormat = do
  s <- get
  case formatSpec s of
    Nothing -> return ()
    Just fs -> do
      fs' <- mskipfmts fs
      s <- get
      put (s {formatSpec = Just fs'})
-}
-- Old version
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Traversing a format = old version with combined do, skip, advance

{-
-- run mdofmt on each format in a list of formats, printing the
-- results as we go, and returning a new list of formats that will
-- contain the futures of all the streams

-- if p then print outputs, otherwise no output.  In either case, the
-- result of advancing the format is returned

mdofmts :: (Signal a, StaticBit a) =>
  Bool -> Bool -> [Format a b] -> StateT (SysState b) IO [Format a b]
mdofmts a p [] = return []
mdofmts a p (x:xs) =
  do x' <- mdofmt a p x
     xs' <- mdofmts a p xs
     return (x':xs')

--  [Format a b] -> StateT (Int,b) IO [Format a b]

-- Process one fmt item

-- process a format item by printing the value for the current clock
-- cycle, and reurn the format for the future clock cycles
-- (essentially, this means to print the head of the stream, and
-- return the tail)

-- this is the key operation in the old-style driver.  It processes
-- the current value and returns the future value

-- mdofmt :: (Signal a, StaticBit a)
--   => Format a b -> StateT (Int,b) IO (Format a b)

mdofmt :: (Signal a, StaticBit a)
  => Bool -> Bool -> Format a b -> StateT (SysState b) IO (Format a b)

mdofmt a p (FmtCall op) = do
  liftIO $ putStrLn "mdofmt FmtCall"
  op
  return (FmtCall op)

-- It will set halted regardless of p
mdofmt a p FmtSetHalted = do
  case a of
    False -> return ()
    True -> do
      s <- get
      put (s {halted = True})
  return FmtSetHalted

--  do mds <- getUserState
--     case mds of
--       Just ds -> do
--         let ds' = ds {Halted = True}
--         putUserState ds'
--         return FmtSetHalted
--       Nothing -> return FmtSetHalted

mdofmt a p (FmtString s) =
--  do condp p $ lift $ putStr s
  do condp p $ lift $ putLogStr s
     return (FmtString s)

mdofmt a p (FmtBit f lbl x) =
  do let (s,x') = f lbl x
     condp p $ lift $ putLogStr s
     return (FmtBit f lbl x')

mdofmt a p (FmtHex f k x) =
  do let (s,x') = f k x
     condp p $ lift $ putLogStr s
     return (FmtHex f k x')

mdofmt a p (FmtWord f lbl w k x) =
  do let (s,x') = f lbl w k x
     condp p $ lift $ putLogStr s
     return (FmtWord f lbl w k x')

mdofmt a p (FmtState f) =
  do mds <- getUserState  -- user's driver state
     case mds of
       Just ds -> do condp p $ lift $ putLogStr (f ds)
                     return (FmtState f)
       Nothing -> return (FmtState f)

mdofmt a p (FmtSetStateWords f xs) =
  do mds <- getUserState
     case mds of
       Just ds -> do
         case a of
           True -> do
             let ws = map (map current) xs
             let ds' = f ds ws
             putUserState ds'
           False -> return ()
       Nothing -> return ()
     return (FmtSetStateWords f (map (map future) xs))

mdofmt a p (FmtSetStateWordsIO f xs) =
  do mds <- getUserState
     case mds of
       Just ds -> do
         case a of
           True -> do
             let ws = map (map current) xs
             f ws
--             let ds' = f ds ws
--             putUserState ds'
           False -> return ()
       Nothing -> return ()
     return (FmtSetStateWordsIO f (map (map future) xs))

mdofmt a p (FmtWordsGeneral f xs) =
  do mds <- getUserState
     case mds of
       Just ds -> do
         let ys = map (map (sigInt . current)) xs
         let s = f ys
         condp p $ lift $ putLogStr s
       Nothing -> return ()
     return (FmtWordsGeneral f (map (map future) xs))

mdofmt a p (FmtIf x fs_then fs_else) =
  do let b = is1 (current x)
     fs_then' <- mdofmts (a && b) (p && b) fs_then
     fs_else' <- mdofmts (a && not b) (p && not b) fs_else
     return (FmtIf (future x) fs_then' fs_else')
--     fs_then' <- (if b then mdofmts else mskipfmts) fs_then
--     fs_else' <- (if b then mskipfmts else mdofmts) fs_else
--     return (FmtIf (future x) fs_then' fs_else')

mdofmt a p (FmtCycle f) =
  do s <- get
     let cycle = cycleCount s
     condp p $ lift $ putLogStr (f cycle)
     return (FmtCycle f)

-- Skip current cycle infor for a list of formats, return futures

mskipfmts :: (Signal a, Static a) =>
  [Format a b] -> StateT (SysState b) IO [Format a b]
mskipfmts [] = return []
mskipfmts (x:xs) =
  do x' <- mskipfmt x
     xs' <- mskipfmts xs
     return (x':xs')

mskipfmt
  :: (Signal a, Static a)
  => Format a b -> StateT (SysState b) IO (Format a b)

mskipfmt FmtSetHalted = return FmtSetHalted
mskipfmt (FmtCycle f) = return (FmtCycle f)
mskipfmt (FmtString s) = return (FmtString s)
mskipfmt (FmtState f) = return (FmtState f)
mskipfmt (FmtSetStateWords f xs) =
  return (FmtSetStateWords f (map (map future) xs))
mskipfmt (FmtBit f lbl x) =
  return (FmtBit f lbl (future x))
mskipfmt (FmtWord f lbl w k x) =
  return (FmtWord f lbl w k (map future x))
mskipfmt (FmtHex f k x) =
  return (FmtHex f k (map future x))
mskipfmt (FmtIf x fs_then fs_else) =
  do fs_then' <- mskipfmts fs_then
     fs_else' <- mskipfmts fs_else
     return (FmtIf (future x) fs_then' fs_else')
mskipfmt (FmtWordsGeneral f xs) =
  return (FmtWordsGeneral f (map (map future) xs))

-- End of old version of format traversal

condp :: Bool -> StateT (SysState a) IO () -> StateT (SysState a) IO ()
condp p action =
  case p of
    True -> action
    False -> return ()

-}


-- old format version end
-- ********************************************************************************

