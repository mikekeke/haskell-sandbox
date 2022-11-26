module App.IdService where

import Data.UUID.V4 (nextRandom)
import App.Types (App, AppEnv)
import Repos (IdService (nextCargoId))
import Types (CargoId (CargoId))
import Control.Concurrent (threadDelay)
import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)

newtype IdSvcIoWithDelayImpl m a = IdSvcIoWithDelayImpl (App m a)
  deriving newtype (
      Functor 
    , Applicative
    , Monad
    , MonadReader AppEnv
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    )
instance MonadIO m => IdService (IdSvcIoWithDelayImpl m) where
  nextCargoId = liftIO $ do
    putStrLn "Getting new ID"
    threadDelay 2_000_000
    Right . CargoId <$> nextRandom
