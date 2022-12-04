module Network
  ( Network,
    getNode,
    nodeIsRunning,
    addNode,
    removeNode,
    newIndex,
  )
where

import Control.Concurrent (Chan)
import Data.Map qualified as M
import Node (NodeId, RunningNode, nodeId, addPeer, showN)
import Data.Maybe (fromJust)

type NetIndex = Map NodeId RunningNode

type Network a = StateT NetIndex IO a

-- getNode :: NodeId -> Network -> Maybe RunningNode
getNode :: NodeId -> Network (Maybe RunningNode)
getNode i = gets (M.lookup i)

nodeIsRunning :: NodeId -> Network Bool
nodeIsRunning = (isJust <$>) . getNode

addNode :: RunningNode -> Network ()
addNode rn = do
  liftIO $ print $ "Adding " <> showN rn
  connect rn
  modify $ M.insert (nodeId rn) rn


removeNode :: RunningNode -> Network ()
removeNode rn = modify (M.delete $ nodeId rn)

newIndex :: Map NodeId RunningNode
newIndex = M.empty


connect :: RunningNode -> Network ()
connect newNode = do
  nodes <- gets M.elems
  forM_ nodes $ \node -> do
    liftIO $ addPeer newNode node
    liftIO $ addPeer node newNode

