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
import Node (NodeId, RunningNode, nodeId)

type NetIndex = Map NodeId RunningNode

data NetworkState = NetworkState
  { bus :: (Chan Message, Chan Message),
    index :: NetIndex
  }

type Network a = StateT NetworkState IO a

-- getNode :: NodeId -> Network -> Maybe RunningNode
getNode :: NodeId -> Network (Maybe RunningNode)
getNode i = gets (M.lookup i . index)

nodeIsRunning :: NodeId -> Network Bool
nodeIsRunning = (isJust <$>) . getNode

addNode :: RunningNode -> Network ()
addNode rn = modify $ \ns -> ns {index = M.insert (nodeId rn) rn (index ns)}

removeNode :: RunningNode -> Network ()
removeNode rn = modify $ \ns -> ns {index = (M.delete $ nodeId rn) (index ns)}

newIndex :: Map NodeId RunningNode
newIndex = M.empty

getIn :: Network (Chan Message)
getIn = gets (fst . bus)

connect :: NodeId -> NodeId -> Network ()
connect i1 i2 = do
  n1 <- getNode i1
  n2 <- getNode i2
  undefined

