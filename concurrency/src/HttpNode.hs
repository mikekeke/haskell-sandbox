module HttpNode (startHttpNode, startDebugNode) where

import Control.Concurrent (ThreadId, forkIO)
import Data.Aeson (ToJSON)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Simple
  ( Response,
    httpNoBody,
    parseRequest_,
    setRequestBodyJSON,
  )
import Node
  ( Message (AddTx, Fetch),
    NodeId,
    OMessage (MyPeers),
    Peer,
    addPeerDebug,
    listenNode,
    nodeId,
    startNode,
    tellNode',
  )
import Types
import Web.Scotty qualified as S

startHttpNode :: Int -> NodeId -> TickView -> IO ThreadId
startHttpNode port nID tickV = forkIO $ do
  node <- startNode tickV nID >>= addPeerDebug "8888"

  listenNode node outMsgHandler

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

outMsgHandler :: OMessage -> IO ()
outMsgHandler = \case
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