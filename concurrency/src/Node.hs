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
    debugPeers,
    debugParent,
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
    threadDelay,
    writeChan,
  )
import Crypto.Hash.SHA256 (hash)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 (encode)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Sequence ((|>))
import Data.Set qualified as S
import LogUtils
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
  { parentNodeAddr :: NodeAddr,
    announceTime :: Int,
    announcers :: NonEmpty NodeAddr,
    parentNodeId :: Text
  }
  deriving stock (Show, Generic)

sameOnTime :: Parent -> Parent -> Bool
sameOnTime p1 p2 =
  ((==) `on` announceTime) p1 p2
    && ((==) `on` parentNodeAddr) p1 p2

type ParentAnnounces = Map NodeAddr Parent

data Node = Node
  { nodeAddr :: NodeAddr,
    inChan :: Chan Message,
    outChan :: Chan OMessage,
    nState :: NodeState,
    nodeTids :: MVar [ThreadId],
    tickView :: TickView,
    nodeHashId :: MVar Text
  }

nodeId = nodeAddr

readId = readMVar . nodeHashId

nodeNetEqual n1 n2 =
  ((==) `on` nodeAddr) n1 n2
    && ((==) `on` nodeHashId) n1 n2

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
  dLog Switch $ show node <> " <SWTICHES> to parent " <> show p
  modifyMVar_
    (nState node)
    (\s -> pure $ s {nodeParent = Just p})

resetParent :: Node -> IO ()
resetParent node = do
  modifyMVar_
    (nState node)
    (\s -> pure $ s {nodeParent = Nothing})

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
  nodeHashId <- newMVar ""
  let node = Node {..}
  idTid <- idThread node
  nodeTids' <- handleNodeProcess node >>= newMVar . (idTid :)
  return $ node {nodeTids = nodeTids'}

idThread :: Node -> IO ThreadId
idThread node = forkIO $ do
  assignId
  forever $ do
    threadDelay 30_000_000
    assignId
  where
    assignId = do
      currSlot <- readMVar (tickView node)
      let addr = nodeAddr node
          !nId = show $ encode $ hash (show addr <> show currSlot)
      void $ swapMVar (nodeHashId node) nId
      void $ resetParent node
      putStrLn $ "Hash updated : " <> show node <> " -> " <> show nId
      tellNode' node AnnounceRoot

handleNodeProcess :: Node -> IO [ThreadId]
handleNodeProcess node = do
  let ns = nState node
      addr = nodeAddr node
  print (mconcat [show node, ": starts listening inbox"] :: Text)
  t1 <- forkIO $
    forever $ do
      msg <- readChan (inChan node)
      dLog Receive (mconcat [show node, ": received ", show msg])
      case msg of
        AddTx tx -> addTx tx ns
        IncomingPeer p -> addPeer p node >> transmit node (PeerIsOk addr p)
        ReceiveParent p -> selectParent node p
        AnnounceRoot -> announceParentToPeers node -- FIXME: better naming and logic
        PeerAccepted p -> addPeer p node
        InitConnection to -> transmit node (RequestConnection addr to)
  return [t1]

listenNode :: Node -> (OMessage -> IO a) -> IO ()
listenNode n handle = do
  listenChan <- dupChan (outChan n)
  tid <- forkIO $ do
    forever $ do
      out <- readChan listenChan
      dLog Send $ show n <> " sending " <> show out
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
  unless (S.null receivers) $ do
    currSlot <- getSlot n
    maybeParent <- getParent n
    hashId <- readId n
    let announcer = nodeAddr n
        parent = case maybeParent of
          Nothing -> Parent (nodeAddr n) currSlot (NE.singleton announcer) hashId
          Just p -> p {announcers = NE.cons announcer (announcers p)}
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
        hashId <- readId node
        pure $ Parent (nodeAddr node) currSlot (NE.singleton $ nodeAddr node) hashId
      Just p -> pure p

  if parentNodeAddr newParent == nodeAddr node
    || sameOnTime newParent currentParent
    then do
      dLog DiscardLoop $ show node <> " discarding parent announce loop"
      pure ()
    else do
      if isBetter currentParent newParent
        then do
          -- reply back to sender with own root
          -- _ <- threadDelay 2_000_000
          announceParentTo node (S.singleton $ head $ announcers newParent)
        else do
          setParent node newParent
          peers <- getPeers node
          -- _ <- threadDelay 2_000_000
          announceParentTo node peers
  where
    isBetter current new =
      -- (announceTime current >= announceTime new)
      --   -- && (announcer current /= announcer new)
      --   &&
      (parentNodeId current <= parentNodeId new)

debugPeers = getPeers

debugParent = getParent