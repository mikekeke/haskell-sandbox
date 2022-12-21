{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Node
  ( Node,
    nodeId,
    PeerAddr,
    startNode,
    tellNode,
    killNode,
    NodeId,
    tellNode',
    Message (..),
    OMessage (..),
    Peer,
    addPeerDebug,
    listenNode,
  )
where

import Control.Concurrent
  ( Chan,
    ThreadId,
    dupChan,
    forkIO,
    killThread,
    modifyMVar_,
    newChan,
    readChan,
    writeChan,
  )
import Data.Sequence
import Text.Show qualified
import Types

type NodeId = Int

data Message
  = AddTx Tx
  | AddPeer Peer
  | Fetch Peer
  -- ChainOf NodeId Text
  deriving stock (Show)

data OMessage
  = MyPeers Peer [Peer]
  deriving stock (Show)

type Tx = Text

type Peer = Text

data Node = Node
  { nodeId :: NodeId,
    inChan :: Chan Message,
    outChan :: Chan OMessage,
    nState :: NodeState,
    nodeTids :: MVar [ThreadId],
    tickView :: TickView
  }

type NodeState = MVar NState

data NState = NState
  { peers :: [Peer],
    transactions :: !(Seq Tx)
  }

addTx :: Tx -> NodeState -> IO ()
addTx tx ns = do
  modifyMVar_
    ns
    (\s -> pure $ s {transactions = transactions s |> tx})

addPeer :: Peer -> NodeState -> IO ()
addPeer p ns = do
  {- Doing like so probably not that critical here and can be done
  same way as `addTx`, it's just to memorize better example from PCPH.
  It could be good for expensive operations in place of `newPeers = p : ps`.
  Also see https://stackoverflow.com/questions/72262784/haskell-strict-mvar-with-bang-pattern
  -}
  {- 
  (NState ps txs) <- takeMVar ns
  let newPeers = p : ps
  putMVar ns (NState newPeers txs)
  seq newPeers (pure ())

  UPD: hmm, it ^ is not safe coz of async exceptions looks like,
       going back to `modifyMVar_`

      maybe something like this?
      peers' <- readMVar ...
      let !newPeers = p : peers`
      modifyMVar_ ns
        (\s -> pure $ s {peers = newPeers})
  -}
  modifyMVar_
    ns
    (\s -> pure $ s {peers = p : peers s})

addPeerDebug :: Peer -> Node -> IO Node
addPeerDebug p node = addPeer p (nState node) >> pure node

instance Show Node where
  show n = "Node #" <> show (nodeId n)

-- showN rn = "Node #" <> show (nodeId rn) -- fixme

tellNode :: Node -> Message -> IO ()
tellNode n = writeChan (inChan n)

tellNode' :: MonadIO m => Node -> Message -> m ()
tellNode' = (liftIO .) . tellNode

startNode :: TickView -> NodeId -> IO Node
startNode tickView nodeId = do
  inChan <- newChan
  outChan <- newChan
  nState <- newMVar (NState mempty mempty)
  nodeTids <- startNodeProcess nodeId inChan outChan nState >>= newMVar
  return $ Node {..}

startNodeProcess :: NodeId -> Chan Message -> Chan OMessage -> NodeState -> IO [ThreadId]
startNodeProcess i inCh outCh ns = do
  print (mconcat ["Node #", show i, ": starts listening inbox"] :: Text)
  t1 <- forkIO $
    forever $ do
      msg <- readChan inCh
      print (mconcat ["Node #", show i, ": received ", show msg] :: Text)
      case msg of
        AddTx tx -> addTx tx ns
        AddPeer p -> addPeer p ns
        Fetch reportTo -> readMVar ns >>= writeChan outCh . MyPeers reportTo . peers
  return [t1]

listenNode :: Node -> (OMessage -> IO a) -> IO ()
listenNode n handle = do
  listenChan <- dupChan (outChan n)
  tid <- forkIO $ do
    forever $ do
      out <- readChan listenChan
      putStrLn $ "Node #" <> show (nodeId n) <> " sending " <> show out
      handle out
  modifyMVar_ (nodeTids n) (pure . (tid :))

killNode :: Node -> IO ()
killNode n = readMVar (nodeTids n) >>= mapM_ killThread

-- filterById i = filter ((== i) . nodeId)

-- sendTo p = writeChan (inbox p)

-- startNodeSendDebug :: IO RunningNode
-- startNodeSendDebug = do
--   print "Starting 666"
--   let nId = 666
--   inb <- newChan
--   peers <- newMVar []
--   t1 <- forkIO . forever $ do
--     let targetId = 1
--     peers' <- readMVar peers
--     print $ "peers " <> show (map showN peers')
--     forM_
--       (filterById targetId peers')
--       (\peer -> do
--         print $ show nId <> " is fetching " <> show targetId
--         sendTo peer (Fetch 1)
--       )
--     threadDelay 2_000_000
--   return $ RunningNode nId inb peers [t1]

-- addPeer :: RunningNode -> RunningNode -> IO ()
-- addPeer self other =
--   modifyMVar_ (nodePeers self)
--   (pure . (other:))

-- removePeer :: RunningNode -> RunningNode -> IO ()
-- removePeer self other =
--   modifyMVar_ (nodePeers self)
--   (pure . filter (/= other))
