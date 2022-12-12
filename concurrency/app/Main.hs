module Main where

import Text.Read (read)
import Data.Text qualified as T

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  putStrLn "Starting"
--   evalStateT runner newIndex

-- runner :: Network ()
-- runner = do 
--   n666 <- liftIO $ startNodeSendDebug
--   addNode n666
--   forever $ do
--     cmd <- getLine
--     case words cmd of
--         ["rn", i] -> do
--           let nId = read (T.unpack i)
--           isRunning <- nodeIsRunning nId
--           unless isRunning $ do
--             print $ "Starting new node " <> i
--             nNode <- liftIO (createNode nId)
--             addNode nNode

--         -- ["kn", i] -> do
--         --   let nId = read (T.unpack i)
--         --   mNode <- gets (getNode nId)
--         --   case mNode of 
--         --     Just node -> do
--         --       print $ "Killing node " <> i
--         --       liftIO $ killNode node
--         --       removeNode $ nodeId node
--         --     _ -> print "Can't kill node - not found"

--         _ -> print "Unknown command"
