module Main where

import Text.Read (read)
import Data.Text qualified as T
import Control.Concurrent (forkIO, threadDelay, modifyMVar_)
import HttpNode (startHttpNode, startDebugNode)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  putStrLn "Enter command:"
  ticker <- startTicker
  let run = runCmd ticker
  mapM_ run ["sn 3001", "sn 3002"]
  forever $ do
    cmd <- getLine
    run cmd

  where
    runCmd ticker cmd = 
      case words cmd of
        ["sn", p] -> startHttpNode (readT p) ticker
        ["sdn"] -> startDebugNode ticker
        _ -> die "hard"

readT :: Read a => Text -> a
readT = read . T.unpack

startTicker :: IO (MVar Int)
startTicker = do
  t <- newMVar 0
  _ <- forkIO $ forever $ modifyMVar_ t (pure . succ) >> threadDelay 1_000_000
  return t

