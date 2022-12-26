{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Node
  ( Node,
    NodeAddr,
    PeerAddr,
    startNode,
    tellNode,
    killNode,
    nodeAddr,
    tellNode',
    Message (..),
    OMessage (..),
    listenNode,
    nodeParent,
    Parent (..),
    getPeers,
    getParent,
    transmit,
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
import Data.Map qualified as M
import Data.Sequence
import Data.Set qualified as S
import Text.Show qualified
import Types

{- TODO
 peers connect
 root announcements

-}

type NodeAddr = Int

data Message
  = AddTx Tx
  | ReceiveParent Parent
  | AnnounceRoot
  | InitConnection NodeAddr
  | IncomingPeer NodeAddr
  | PeerAccepted NodeAddr -- by whom
  -- ChainOf NodeAddr Text
  deriving stock (Show)

data OMessage
  = MyParent (Set NodeAddr) Parent
  | PeerIsOk NodeAddr NodeAddr -- this node id, peer id
  | RequestConnection NodeAddr NodeAddr
  deriving stock (Show)

type Tx = Text

data Parent = Parent
  { announceTime :: Int,
    announcer :: NodeAddr,
    parentNode :: NodeAddr
  }
  deriving stock (Show, Generic)

type ParentAnnounces = Map NodeAddr Parent

data Node = Node
  { nodeAddr :: NodeAddr,
    inChan :: Chan Message,
    outChan :: Chan OMessage,
    nState :: NodeState,
    nodeTids :: MVar [ThreadId],
    tickView :: TickView
  }

type NodeState = MVar NState

data NState = NState
  { announces :: ParentAnnounces, -- ? TODO: maybe they should go to HttpNode?
    transactions :: !(Seq Tx),
    nodeParent :: Maybe Parent,
    peers :: Set NodeAddr
  }

getParent :: Node -> IO (Maybe Parent)
getParent = fmap nodeParent . readMVar . nState

getPeers :: Node -> IO (Set NodeAddr)
getPeers = fmap peers . readMVar . nState

setParent :: Node -> Parent -> IO ()
setParent node p = do
  putStrLn $ "Node #" <> show (nodeAddr node) <> " swtiches to parent " <> show p
  modifyMVar_
    (nState node)
    (\s -> pure $ s {nodeParent = Just p})

addTx :: Tx -> NodeState -> IO ()
addTx tx ns = do
  modifyMVar_
    ns
    (\s -> pure $ s {transactions = transactions s |> tx})

addPeer :: NodeAddr -> Node -> IO ()
addPeer p n = do
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
    (nState n)
    (\s -> pure $ s {peers = S.insert p (peers s)})

instance Show Node where
  show n = "Node #" <> show (nodeAddr n)

-- showN rn = "Node #" <> show (NodeAddr rn) -- fixme

tellNode :: Node -> Message -> IO ()
tellNode n = writeChan (inChan n)

tellNode' :: MonadIO m => Node -> Message -> m ()
tellNode' = (liftIO .) . tellNode

transmit :: MonadIO m => Node -> OMessage -> m ()
transmit node msg = liftIO $ writeChan (outChan node) msg

startNode :: TickView -> NodeAddr -> IO Node
startNode tickView nodeAddr = do
  inChan <- newChan
  outChan <- newChan
  nState <- newMVar (NState mempty mempty Nothing mempty)
  nodeTids <- newMVar []
  let node = Node {..}
  nodeTids' <- handleNodeProcess node >>= newMVar
  return $ node {nodeTids = nodeTids'}

handleNodeProcess :: Node -> IO [ThreadId]
handleNodeProcess node = do
  let ns = nState node
      i = nodeAddr node
  print (mconcat ["Node #", show i, ": starts listening inbox"] :: Text)
  t1 <- forkIO $
    forever $ do
      msg <- readChan (inChan node)
      print (mconcat ["Node #", show i, ": received ", show msg] :: Text)
      case msg of
        AddTx tx -> addTx tx ns
        IncomingPeer p -> addPeer p node >> transmit node (PeerIsOk i p)
        ReceiveParent p -> selectParent node p
        AnnounceRoot -> announceParentToPeers node -- FIXME: better naming and logic
        PeerAccepted p -> addPeer p node
        InitConnection to -> transmit node (RequestConnection i to)
  return [t1]

listenNode :: Node -> (OMessage -> IO a) -> IO ()
listenNode n handle = do
  listenChan <- dupChan (outChan n)
  tid <- forkIO $ do
    forever $ do
      out <- readChan listenChan
      putStrLn $ "Node #" <> show (nodeAddr n) <> " sending " <> show out
      handle out
  modifyMVar_ (nodeTids n) (pure . (tid :))

killNode :: Node -> IO ()
killNode n = readMVar (nodeTids n) >>= mapM_ killThread

announceParentToPeers :: Node -> IO ()
announceParentToPeers n = do
  peers <- getPeers n
  announceParentTo n peers

announceParentTo :: Node -> Set NodeAddr -> IO ()
announceParentTo n receivers = do
  currSlot <- getSlot n
  maybeParent <- getParent n
  let announcer = nodeAddr n
      parent = case maybeParent of
        Nothing -> Parent currSlot announcer (nodeAddr n)
        Just p -> p
  transmit n $ MyParent receivers parent

getSlot :: Node -> IO Int
getSlot n = readMVar (tickView n)

selectParent :: Node -> Parent -> IO ()
selectParent node newParent = do
  mParent <- getParent node
  currentParent <-
    case mParent of
      Nothing -> do
        currSlot <- getSlot node
        pure $ Parent currSlot (nodeAddr node) (nodeAddr node)
      Just p -> pure p

  if parentNode newParent == nodeAddr node
    then do
      putStrLn $
        "Node #" <> show (nodeAddr node)
          <> " discarding parent announce loop"
      pure ()
    else do
      -- putStrLn $ "Node #" <> show currentParent
      -- putStrLn $ "Node #" <> show newParent
      unless (isBetter currentParent newParent) $ do
        setParent node newParent
      peers <- getPeers node
      announceParentTo node (S.insert (announcer newParent) peers)
  where
    isBetter current new =
      -- (announceTime current >= announceTime new)
      --   -- && (announcer current /= announcer new)
      --   &&
      (parentNode current < parentNode new)
