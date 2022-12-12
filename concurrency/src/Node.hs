{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Node
  ( Node,
    nodeId,
    inChan,
    outChan,
    PeerAddr,
    startNode,
    tellNode,
    killNode,
    NodeId,
    tellNode',
    Message (..),
  )
where

import Control.Concurrent
  ( Chan,
    ThreadId,
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
  -- ChainOf NodeId Text
  deriving stock (Show)

type Tx = Text

data Node = Node
  { nodeId :: NodeId,
    inChan :: Chan Message,
    outChan :: Chan Message,
    nState :: NodeState,
    nodeTids :: [ThreadId],
    tickView :: TickView
  }

type NodeState = MVar NState

data NState = NState
  { peers :: [Text],
    transactions :: Seq Tx
  }

addTxToState :: Tx -> NodeState -> IO ()
addTxToState tx ns = do
  modifyMVar_
    ns
    (\s -> pure $ s {transactions = transactions s |> tx})

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
  nodeTids <- startNodeProcess nodeId inChan nState
  return $ Node {..}

startNodeProcess :: NodeId -> Chan Message -> NodeState -> IO [ThreadId]
startNodeProcess i inCH ns = do
  print (mconcat ["Node #", show i, ": starts listening inbox"] :: Text)
  t1 <- forkIO $
    forever $ do
      msg <- readChan inCH
      print (mconcat ["Node #", show i, ": received ", show msg] :: Text)
      case msg of
        AddTx tx -> addTxToState tx ns
  return [t1]

killNode :: Node -> IO ()
killNode = mapM_ killThread . nodeTids

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
