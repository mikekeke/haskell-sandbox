module HttpNode (startHttpNode) where

import Web.Scotty qualified as S
import Control.Concurrent (ThreadId, forkIO)
import Data.UUID.V4 (nextRandom)
import Data.UUID qualified as UUID
import Types
import Node (NodeId, startNode, tellNode', Message (AddTx))

startHttpNode :: Int -> NodeId -> TickView -> IO ThreadId 
startHttpNode port nID tickV = forkIO $ do
  node <- startNode tickV nID
  print $ "HttpNode started: " <> show node
  S.scotty port $
    S.post "/add-tx" $ do
      uid <- UUID.toText <$> liftIO nextRandom 
      tellNode' node (AddTx uid)
      S.json @Text "Ok"