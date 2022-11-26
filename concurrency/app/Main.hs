{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Control.Concurrent (Chan, newChan, threadDelay, writeChan, dupChan, readChan, forkIO)
import Control.Concurrent.Async (poll, withAsync)
import System.IO (getChar)
import Data.Text qualified as T

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  putStrLn "Starting"
  chan <- newChan
  _ <- forkIO $ runApp newApp (AppEnv chan)
  listenChan <- dupChan chan
  fix $ \loop -> do
    event <- readChan listenChan
    case event of
      QuitEvent -> putStrLn "Done"
      other -> print other >> threadDelay 1_000_000 >> loop

class EventBus app where
  type EventType app :: Type
  type BusType app :: Type

  sendToBus :: EventType app -> app ()

data Event
  = SomeEvent Text
  | QuitEvent
  deriving stock (Show)

instance IsString Event where
  fromString = SomeEvent . T.pack

newtype App a = App {getApp :: ReaderT AppEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader AppEnv, MonadIO)

data AppEnv = AppEnv
  { eventBus :: BusType App
  }

instance EventBus App where
  type EventType App = Event
  type BusType App = Chan Event

  sendToBus ev = asks eventBus >>= \ch -> liftIO $ writeChan ch ev

runApp :: App a -> AppEnv -> IO a
runApp = runReaderT . getApp

newApp :: App ()
newApp = go
  where
    events = ["lol", "kek", "run", "user logged in", "we are hacked"] ++ [QuitEvent]
    go = do
      forM_ events $ \event -> do
        sendToBus event
        liftIO $ threadDelay 200_000
      print @Text "Sending finished"
