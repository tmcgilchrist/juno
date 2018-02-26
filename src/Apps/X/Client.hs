{-# LANGUAGE OverloadedStrings #-}

module Apps.X.Client
  ( mainBase
  , mainRepl
  , mainProgrammatic
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.Lifted (threadDelay)
import qualified Control.Concurrent.Lifted as CL
import Control.Concurrent.Chan.Unagi
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSC
import Data.Either ()
import qualified Data.Map as Map
import System.IO
import GHC.Int (Int64)

import Juno.Spec.Simple
import Juno.Types

import Apps.X.Parser

promptIn :: String
promptIn = "\ESC[0;31min>> \ESC[0m"

promptOut :: String
promptOut = "\ESC[0;32mout>> \ESC[0m"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

promptRead :: IO String
promptRead = flushStr promptIn >> getLine

-- should we poll here till we get a result?
showResult :: CommandMVarMap -> RequestId -> Maybe Int64 -> IO ()
showResult cmdStatusMap' rId Nothing =
  threadDelay 1000 >> do
    (CommandMap _ m) <- readMVar cmdStatusMap'
    case Map.lookup rId m of
      Nothing -> print $ "RequestId [" ++ show rId ++ "] not found."
      Just (CmdApplied (CommandResult x) _) -> putStrLn $ promptOut ++ BSC.unpack x
      -- not applied yet, loop and wait
      Just _ -> showResult cmdStatusMap' rId Nothing
showResult cmdStatusMap' rId pgm@(Just cnt) =
  threadDelay 1000 >> do
    (CommandMap _ m) <- readMVar cmdStatusMap'
    case Map.lookup rId m of
      Nothing -> print $ "RequestId [" ++ show rId ++ "] not found."
      Just (CmdApplied (CommandResult _x) lat) -> putStrLn $ intervalOfNumerous cnt lat
      -- not applied yet, loop and wait
      Just _ -> showResult cmdStatusMap' rId pgm

--  -> OutChan CommandResult
runREPL :: InChan (RequestId, [CommandEntry]) -> CommandMVarMap -> IO ()
runREPL toCommands' cmdStatusMap' = do
  cmd <- promptRead
  case cmd of
    "" -> runREPL toCommands' cmdStatusMap'
    "program" -> runProgram toCommands' cmdStatusMap'
    _ -> do
      let cmd' = BSC.pack cmd
      case readX cmd' of
        Left err -> putStrLn cmd >> putStrLn err >> runREPL toCommands' cmdStatusMap'
        Right _ -> do
          rId <- liftIO $ setNextCmdRequestId cmdStatusMap'
          writeChan toCommands' (rId, [CommandEntry cmd'])
          showResult cmdStatusMap' rId Nothing
          runREPL toCommands' cmdStatusMap'

runProgram :: InChan (RequestId, [CommandEntry]) -> CommandMVarMap -> IO ()
runProgram toCommands' cmdStatusMap' = runProgram' (0::Int) (0::Int)
 where
  runProgram' i j = do
    threadDelay 15000 -- 1000000
    rId <- liftIO $ setNextCmdRequestId cmdStatusMap'
    writeChan toCommands' (rId, [CommandEntry entry])
    showResult cmdStatusMap' rId Nothing
    runProgram' (i+1) (if i `mod` 4 == 0 then j+1 else j)
   where
    entry = BSC.pack $ case i `mod` 4 of
      0 -> "create "  ++ show (j+1)
      1 -> "showone " ++ show j
      2 -> "adjust "  ++ show j ++ " " ++ show j
      _ -> "showone " ++ show j

intervalOfNumerous :: Int64 -> Int64 -> String
intervalOfNumerous cnt mics = let
  interval = fromIntegral mics / 1000000
  perSec = ceiling (fromIntegral cnt / interval)
  in "Completed in " ++ show (interval :: Double) ++ "sec (" ++ show (perSec::Integer) ++ " per sec)"

-- | Runs a 'Raft nt String String mt'.
-- Simple fixes nt to 'HostPort' and mt to 'String'.
mainRepl :: IO ()
mainRepl = mainBase runREPL

mainProgrammatic :: IO ()
mainProgrammatic = mainBase runProgram

mainBase
  :: (InChan (RequestId, [CommandEntry]) -> CommandMVarMap -> IO ())
  -> IO ()
mainBase driver = do
  (toCommands, fromCommands) <- newChan
  -- `toResult` is unused. There seem to be API's that use/block on fromResult.
  -- Either we need to kill this channel full stop or `toResult` needs to be used.
  cmdStatusMap' <- initCommandMap
  let -- getEntry :: (IO et)
      getEntries :: IO (RequestId, [CommandEntry])
      getEntries = readChan fromCommands
      -- applyFn :: et -> IO rt
      applyFn :: Command -> IO CommandResult
      applyFn _x = return $ CommandResult "Failure"
  void $ CL.fork $ runClient applyFn getEntries cmdStatusMap'
  threadDelay 100000
  driver toCommands cmdStatusMap'
