{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
-- import Hop.Apps.Juno.Ledger (runQuery, convertQuery)

-- state
newtype XEnv = XEnv {
  getStateMVar :: MV.MVar (Map.Map Text Rational)
  }

-- X
starterEnv :: IO XEnv
starterEnv = XEnv <$> MV.newMVar Map.empty

runCommand :: XEnv -> Command -> IO CommandResult
runCommand env Command{_cmdEntry = cmd'@_, _cmdRequestId = _} = do
  let mvar = getStateMVar env
  orgState <- MV.takeMVar mvar
  case readX $ unCommandEntry cmd' of
    Left err -> do
      MV.putMVar mvar orgState
      return $ CommandResult $ BSC.pack err
    Right cmd -> fmap CommandResult $ handle
        (\e -> do
            MV.putMVar mvar orgState
            return $ BSC.pack $ show (e :: SomeException)) $
        case cmd of
            CreateAccount acct ->
                if Map.member acct orgState
                then do
                    MV.putMVar mvar orgState
                    return "Account Already Exists"
                else do
                    MV.putMVar mvar (Map.insert acct 0 orgState)
                    return $ BSC.pack $ "Created Account: " ++ show acct

            AdjustAccount acct amount ->
                if Map.member acct orgState
                then do
                    MV.putMVar mvar (Map.adjust (+ amount) acct orgState)
                    return $ BSC.pack $ "Adjusted Account " ++ show acct ++ " by " ++ show amount
                else do
                    MV.putMVar mvar orgState
                    return $ BSC.pack $ "Error: Account " ++ show acct ++ " does not exist!"

            ObserveAccount acct -> do
                MV.putMVar mvar orgState
                return $ BSC.pack $ show $ Map.lookup acct orgState

            ObserveAccounts -> do
                MV.putMVar mvar orgState
                return $ BSC.pack $ prettyLedger orgState

prettyLedger :: Map.Map Text Rational -> String
prettyLedger m = unlines $ Map.foldlWithKey (\l k v -> l ++ [T.unpack k ++ ": " ++ show (fromRational v :: Double)]) ["","Account: Amount", "----------------------"] m
