module HttpNode (startHttpNode, startDebugNode) where

import Control.Concurrent (ThreadId, forkIO)
import Data.Aeson (FromJSON, ToJSON)
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
  ( Message
      ( AddTx,
        IncomingPeer,
        InitConnection,
        PeerAccepted,
        ReceiveParent, AnnounceRoot
      ),
    NodeAddr,
    OMessage
      ( MyParent,
        PeerIsOk,
        RequestConnection
      ),
    Parent,
    debugParent,
    debugPeers,
    listenNode,
    nodeAddr,
    startNode,
    tellNode',
  )
import Types
import Web.Scotty qualified as S

startHttpNode :: NodeAddr -> TickView -> IO ThreadId
startHttpNode port tickV = forkIO $ do
  node <- startNode tickV port -- >>= addPeer 8888
  listenNode node outMsgHandler

  let tellN = tellNode' node
  print $ "HttpNode started: " <> show node
  S.scotty port $ do
    S.post "/add-tx" $ do
      uid <- UUID.toText <$> liftIO nextRandom
      tellN (AddTx uid)
      S.json @Text "Ok"

    S.post "/take-peers" $ do
      receivedPeers :: [NodeAddr] <- S.jsonData
      putStrLn $ "Node #" <> show (nodeAddr node) <> " got peers: " <> show receivedPeers

    S.post "/connect" $ do
      toPort :: NodeAddr <- S.jsonData
      tellN (InitConnection toPort)

    S.post "/take-parent" $ do
      candidate :: Parent <- S.jsonData
      tellNode' node (ReceiveParent candidate)

    S.get "/d-get-peers" $ do
      ps <- liftIO $ debugPeers node
      S.json ps

    S.get "/d-get-parent" $ do
      ps <- liftIO $ debugParent node
      S.json ps

    S.post "/incoming-connect" $ do
      fromAddr :: NodeAddr <- S.jsonData
      tellNode' node (IncomingPeer fromAddr)

    S.post "/accepted" $ do
      accepted :: NodeAddr <- S.jsonData
      tellNode' node (PeerAccepted accepted)
      tellNode' node AnnounceRoot

outMsgHandler :: OMessage -> IO ()
outMsgHandler = \case
  MyParent ps parent -> forM_ ps (\p -> postTo parent p "/take-parent")
  RequestConnection from to -> void $ postTo from to "/incoming-connect"
  PeerIsOk me requester -> void $ postTo me requester "/accepted"
  where
    postTo :: (ToJSON a) => a -> NodeAddr -> Text -> IO (Response ()) -- FIXME: something better than Text?
    postTo payload to method = do
      httpNoBody
        . setRequestBodyJSON payload
        . parseRequest_
        . T.unpack
        $ "POST http://localhost:" <> show to <> method

startDebugNode :: TickView -> IO ThreadId
startDebugNode tv = forkIO $ do
  _ <- startHttpNode 3003 tv
  _ <- httpNoBody "http://localhost:3000/fetch?sender=3003"
  pure ()

instance ToJSON Parent
instance FromJSON Parent