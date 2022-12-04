module Node
  ( RunningNode,
    nodeId,
    createNode,
    killNode,
    NodeId,
    -- startNodeSendDebug,
  addPeer, removePeer, startNodeSendDebug)
where

import Control.Concurrent (Chan, ThreadId, newChan, forkIO, readChan, writeChan, killThread, modifyMVar, modifyMVar_)

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
        Fetch _ -> do
          print $ "Got " <> show msg
          forM_ peers' $ \peer -> do writeChan (inbox peer) (ChainOf i "aaa")
        other -> print $ show i <> " received: " <> show other
  return [t1]

killNode :: RunningNode -> IO ()
killNode = mapM_ killThread . nodeTids

startNodeSendDebug :: IO RunningNode
startNodeSendDebug = do
  let nId = 666
  inb <- newChan
  peers <- newMVar []
  t1 <- forkIO . forever $ do
    peers' <- readMVar peers
    forM_ peers' $ \peer -> do 
      print $ show nId <> " is fetching 1"
      writeChan (inbox peer) (Fetch 1)
  return $ RunningNode nId inb peers [t1]


addPeer :: RunningNode -> RunningNode -> IO ()
addPeer self other =
  modifyMVar_ (nodePeers self)
  (pure . (other:))

removePeer :: RunningNode -> RunningNode -> IO ()
removePeer self other =
  modifyMVar_ (nodePeers self)
  (pure . filter (/= other))
