module IOApp.IdService where

import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import IOApp.Types (IOApp)
import Repos (IdService (nextCargoId))
import Types (CargoId (CargoId))
import Control.Concurrent (threadDelay)

instance IdService IOApp where
  nextCargoId = liftIO $ do
    putStrLn "Getting new ID"
    threadDelay 2_000_000
    Right . CargoId <$> nextRandom