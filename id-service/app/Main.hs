module Main (main) where

import Web.Scotty ( get, json, scotty )
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)

main :: IO ()
main = do
  putStrLn "ID service"
  scotty 3000 $
    get "/new-uid" $ do
      liftIO $ putStrLn "Prcessing UUID request"
      uid <- liftIO nextRandom 
      json uid