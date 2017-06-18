{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Monad            (forM, forM_)
import           Data.List.Utils          (replace)
import           Data.Monoid              ((<>))
import           Data.Thyme.Calendar      (showGregorian)
import           Data.Thyme.LocalTime     (LocalTime (LocalTime),
                                           ZonedTime (ZonedTime), getZonedTime)
import           System.IO                (Handle, IOMode (WriteMode), hFlush,
                                           hPutStrLn, openFile)
import           System.IO.HVIO           (HVIO)
import           System.IO.Utils          (hCopy)
import           System.Process           (ProcessHandle,
                                           StdStream (CreatePipe),
                                           createProcess, proc, readProcess,
                                           std_in, std_out)

configDir = "run/conf"
logDir    = "/tmp"

main = do
  enterBanner '='
  p <- readProcess "pwd" [] ""
  ts <- timestamp
  print ("pwd:" <> p)
  print ("timestamp:" <> ts)
  _ <- startServers ts
  cs <- startClients ts
  sendInputToClients cs
  threadDelay 100000

sendInputToClients cs =
  forM_ cs $ \(_,_,hin) ->
    forM_ [ "create foo"
          , "create bar"
          , "showone foo"
          , "showone bar"
          , "adjust foo 200"
          , "showone foo"
          , "showall"
          ] $ \s -> do
                hPutStrLn hin s
                hFlush hin
                print s

startServers :: String -> IO [(ProcessHandle, Handle)]
startServers ts = do
  enterBanner 's'
  ss <- serverConfigs
  forM ss $ \s -> do
    loopBanner s
    (_, Just hout, _, ph) <- createProcess
      (proc "stack" ["exec", "xserver", "--", "-c", s])
      { std_out = CreatePipe }
    lh <- pipeOutToLog hout s ts
    return (ph, lh)

startClients :: String -> IO [(ProcessHandle, Handle, Handle)]
startClients ts = do
  enterBanner 'c'
  cs <- clientConfigs
  forM cs $ \c -> do
    loopBanner c
    (Just cin, Just hout, _, ph) <- createProcess
      (proc "stack" ["exec", "xclient", "--", "-c", c])
      { std_in  = CreatePipe }
      { std_out = CreatePipe }
    lh <- pipeOutToLog hout c ts
    return (ph, lh, cin)

killServers = undefined

pipeOutToLog :: HVIO a => a -> ConfigFilename -> Timestamp -> IO Handle
pipeOutToLog hout c ts = do
  let fn = configToLogName c ts
  fh <- openFile fn WriteMode
  _ <- async (hCopy hout fh)
  return fh

configToLogName s ts =
  logDir <> "/" <> replace ".yaml" ("-" <> ts <> ".log") (replace "/" "-" s)

type ConfigFilename = String

clientConfigs :: IO [ConfigFilename]
clientConfigs = getConfigs "*client.yaml"

serverConfigs :: IO [ConfigFilename]
serverConfigs = getConfigs "*cluster.yaml"

getConfigs c  = do
  r <- readProcess "find" [configDir, "-name", c, "-print"] ""
  return $ lines r

type Timestamp = String

timestamp :: IO Timestamp
timestamp = do
  (ZonedTime (LocalTime d' t') _) <- getZonedTime
  return $ showGregorian d' <> "T" <> take 15 (show t')

enterBanner s = print (replicate 50 s)

loopBanner s = do
  print "--------------------------------------------------"
  print s

-- End of file.
