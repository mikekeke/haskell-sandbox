module App.IdServiceIo (
  IoIdSvcWithDelay (..)
  ) where

import Control.Concurrent (threadDelay)
import Data.UUID.V4 (nextRandom)
import Repos (IdService (nextCargoId))
import Types (CargoId (CargoId))
import UnliftIO (MonadUnliftIO)

newtype IoIdSvcWithDelay m a = IoIdSvcWithDelay (m a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      -- , MonadReader AppEnv
      MonadIO,
      MonadUnliftIO
      -- , MonadLogger
    )

instance MonadIO m => IdService (IoIdSvcWithDelay m) where
  nextCargoId = liftIO $ do
    putStrLn "Getting new ID"
    threadDelay 2_000_000
    Right . CargoId <$> nextRandom