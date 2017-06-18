{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Apps.X.Command where

import Data.Either ()
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Control.Concurrent.MVar as MV
import Control.Exception (SomeException, handle)

import Juno.Types (CommandEntry(..), CommandResult(..))
import Juno.Types.Message.CMD

import Apps.X.Parser

-- state
newtype XEnv = XEnv { getStateMVar :: MV.MVar (Map.Map Text Integer) }

-- X
starterEnv :: IO XEnv
starterEnv = XEnv <$> MV.newMVar Map.empty

runCommand :: XEnv -> Command -> IO CommandResult
runCommand env Command{_cmdEntry = cmd'@_, _cmdRequestId = _} = do
  let mvar = getStateMVar env
  origState <- MV.takeMVar mvar
  case readX $ unCommandEntry cmd' of
    Left err -> do
      MV.putMVar mvar origState
      return $ CommandResult $ BSC.pack err
    Right cmd -> fmap CommandResult $ handle
        (\e -> do
            MV.putMVar mvar origState
            return $ BSC.pack $ show (e :: SomeException)) $
        case cmd of
            Create acct ->
                if Map.member acct origState then do
                  MV.putMVar mvar origState
                  return "Account Already Exists"
                else do
                  MV.putMVar mvar (Map.insert acct 0 origState)
                  return $ BSC.pack $ "Created Account: " ++ show acct

            Adjust acct amount ->
                if Map.member acct origState then do
                  MV.putMVar mvar (Map.adjust (+ amount) acct origState)
                  return $ BSC.pack $ "Adjusted Account " ++ show acct ++ " by " ++ show amount
                else do
                  MV.putMVar mvar origState
                  return $ BSC.pack $ "Error: Account " ++ show acct ++ " does not exist!"

            ShowOne acct -> do
                MV.putMVar mvar origState
                return $ BSC.pack $ show $ Map.lookup acct origState

            ShowAll -> do
                MV.putMVar mvar origState
                return $ BSC.pack $ prettyLedger origState

prettyLedger :: Map.Map Text Integer -> String
prettyLedger m = unlines $ Map.foldlWithKey
  (\a k v -> a ++ [T.unpack k ++ ": " ++ show (v :: Integer)])
  ["","Account: Amount", "----------------------"]
  m
