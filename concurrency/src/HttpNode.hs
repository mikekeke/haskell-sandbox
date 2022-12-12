module HttpNode (startHttpNode) where

import Web.Scotty qualified as S
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Types

startHttpNode :: Int -> NodeId -> TickView -> IO ()
startHttpNode port tickV = do
  putStrLn "ID service"
  S.scotty port $
    S.post "/add-tx" $ do
      liftIO $ putStrLn "Prcessing UUID request"
      uid <- liftIO nextRandom 
      S.json uid