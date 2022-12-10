{-# LANGUAGE OverloadedStrings #-}
module Node
  ( RunningNode,
    nodeId,
    createNode,
    killNode,
    NodeId,
    showN,
    -- startNodeSendDebug,
  addPeer, removePeer, startNodeSendDebug)
where

import Control.Concurrent (Chan, ThreadId, newChan, forkIO, readChan, writeChan, killThread, modifyMVar, modifyMVar_, threadDelay)
import Data.List.Split
import Text.Read (read)
import Data.Text.IO (getContents)

type NodeId = Int

data Message
  = Fetch NodeId
  | ChainOf NodeId Text
  deriving stock (Show)

data RunningNode = RunningNode
  { nodeId :: NodeId,
    inbox :: Chan Message,
    nodePeers :: MVar [RunningNode],
    nodeTids :: [ThreadId]
  }

showN rn = "Node #" <> show (nodeId rn) -- fixme

instance Eq RunningNode where
  (==) = (==) `on` nodeId

createNode :: NodeId -> IO RunningNode
createNode i = do
  inb <- newChan
  peers <- newMVar []
  tIds <- startNodeProcess i inb peers
  return $ RunningNode i inb peers tIds

startNodeProcess :: Int -> Chan Message -> MVar [RunningNode] -> IO [ThreadId]
startNodeProcess i inb peers = do
  print (mconcat ["Node #", show i, ": starts listening inbox"] :: Text)
  t1 <- forkIO $
    forever $ do
      msg <- readChan inb
      peers' <- readMVar peers
      case msg of
        Fetch reqI -> do
          print $ "Node #" <> show i <> " received: " <> show msg
          forM_
            (filterById reqI peers')
            (\peer -> sendTo peer (ChainOf i "aaa"))
        other -> print $ show i <> " received: " <> show other
  return [t1]

killNode :: RunningNode -> IO ()
killNode = mapM_ killThread . nodeTids

filterById i = filter ((== i) . nodeId)

sendTo p = writeChan (inbox p)

startNodeSendDebug :: IO RunningNode
startNodeSendDebug = do
  print "Starting 666"
  let nId = 666
  inb <- newChan
  peers <- newMVar []
  t1 <- forkIO . forever $ do
    let targetId = 1
    peers' <- readMVar peers
    print $ "peers " <> show (map showN peers')
    forM_ 
      (filterById targetId peers') 
      (\peer -> do
        print $ show nId <> " is fetching " <> show targetId
        sendTo peer (Fetch 1)
      )
    threadDelay 2_000_000
  return $ RunningNode nId inb peers [t1]


addPeer :: RunningNode -> RunningNode -> IO ()
addPeer self other =
  modifyMVar_ (nodePeers self)
  (pure . (other:))

removePeer :: RunningNode -> RunningNode -> IO ()
removePeer self other =
  modifyMVar_ (nodePeers self)
  (pure . filter (/= other))


-- Function to parse a string representation of a range of IDs into a pair of Ints
parseRange :: Text -> (Int, Int)
parseRange s = (read a, read b)
    where [a, b] = splitOn "-" s

-- Function to check if one range fully contains the other
rangeContains :: (Int, Int) -> (Int, Int) -> Bool
rangeContains (a1, b1) (a2, b2) = (a1 <= a2) && (b1 >= b2)

-- Main function
main :: IO ()
main = do
    -- Read the input
    input <- getLine

    -- Parse the input into a list of range pairs
    let ranges = map (map parseRange . words) . lines $ input

    -- Count the number of pairs where one range fully contains the other
    let count = length . filter (\[r1, r2] -> rangeContains r1 r2 || rangeContains r2 r1) $ ranges

    -- Print the result
    print count