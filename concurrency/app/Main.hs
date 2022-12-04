module Main where

import Network (Network, newIndex, nodeIsRunning, addNode, removeNode, getNode)
import Text.Read (read)
import Data.Text qualified as T
import Node (createNode, nodeId, killNode)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  putStrLn "Starting"
  evalStateT runner newIndex

runner :: Network ()
runner = forever $ do
  undefined
  -- cmd <- getLine
  -- case words cmd of
  --     ["rn", i] -> do
  --       let nId = read (T.unpack i)
  --       isRunning <- gets (nodeIsRunning nId)
  --       unless isRunning $ do
  --         print $ "Starting new node " <> i
  --         nNode <- liftIO (newNode nId)
  --         modify (addNode nNode)

  --     ["kn", i] -> do
  --       let nId = read (T.unpack i)
  --       mNode <- gets (getNode nId)
  --       case mNode of 
  --         Just node -> do
  --           print $ "Killing node " <> i
  --           liftIO $ killNode node
  --           modify (removeNode $ nodeId node)
  --         _ -> print "Can't kill node - not found"

  --     _ -> print "Unknown command"
