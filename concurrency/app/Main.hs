module Main where

import Text.Read (read)
import Data.Text qualified as T
import Control.Concurrent (forkIO, threadDelay, modifyMVar_)
import HttpNode (startHttpNode)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  putStrLn "Enter command:"
  ticker <- startTicker
  forever $ do
    cmd <- getLine
    case words cmd of
        ["sn", i, p] -> startHttpNode (readT p) (readT i) ticker
        _ -> die "hard"

readT :: Read a => Text -> a
readT = read . T.unpack

startTicker :: IO (MVar Int)
startTicker = do
  t <- newMVar 0
  _ <- forkIO $ modifyMVar_ t (pure . succ) >> threadDelay 1_000_000
  return t

