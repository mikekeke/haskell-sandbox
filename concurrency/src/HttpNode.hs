module HttpNode (startHttpNode, startDebugNode) where

import Control.Concurrent (ThreadId, forkIO, readChan)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Simple (Response, httpJSON, parseRequest, parseRequest_, setRequestBodyJSON, httpNoBody)
import Node
  ( Message (AddTx, Fetch),
    Node,
    NodeId,
    OMessage (MyPeers),
    Peer,
    outChan,
    startNode,
    tellNode', nodeId, addPeerDebug,
  )
import Types
import Web.Scotty qualified as S

startHttpNode :: Int -> NodeId -> TickView -> IO ThreadId
startHttpNode port nID tickV = forkIO $ do
  node <- startNode tickV nID >>= addPeerDebug "8888"

  startListenerThread node
  let tellN = tellNode' node
  print $ "HttpNode started: " <> show node
  S.scotty port $ do
    S.post "/add-tx" $ do
      uid <- UUID.toText <$> liftIO nextRandom
      tellN (AddTx uid)
      S.json @Text "Ok"
    
    S.get "/fetch" $ do
      sender <- S.param "sender"
      tellN (Fetch sender)
  
    S.post "/take-peers" $ do
      receivedPeers :: [Peer] <- S.jsonData
      putStrLn $ "Node #" <> show (nodeId node) <> " got peers: " <> show receivedPeers

startListenerThread :: Node -> IO ()
startListenerThread n = void $
  forkIO $ do
    forever $ do
      out <- readChan $ outChan n
      putStrLn $ "Node #" <> show (nodeId n) <> " sending " <> show out
      case out of
        MyPeers sendTo peers' -> void $ peers' `postTo` sendTo
  where
    postTo :: (ToJSON a) => a -> Peer -> IO (Response ()) -- FIXME: something better than Text?
    postTo payload to = do
      httpNoBody
        . setRequestBodyJSON payload
        . parseRequest_
        . T.unpack
        $ "POST http://localhost:" <> to <> "/take-peers"


startDebugNode :: TickView -> IO ThreadId
startDebugNode tv = forkIO $ do
  _ <- startHttpNode 3003 666 tv
  _ <- httpNoBody "http://localhost:3000/fetch?sender=3003"
  pure ()