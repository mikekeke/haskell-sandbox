module App.IdServiceSubs where

import Data.UUID.V4 (nextRandom)
import Repos (IdService (nextCargoId))
import Types (CargoId (CargoId))
import Control.Concurrent (threadDelay)
import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)


-----------------------------------------------------------------------
-- requires this in another module to avoid cyclic dependency
-- deriving via (IdSvcIoNewtypeWrap  m) 
--   instance MonadIO m => (IdService (App m)) 

-- newtype IdSvcIoNewtypeWrap m a = IdSvcIoNewtypeWrap (App m a)
--   deriving newtype (
--       Functor 
--     , Applicative
--     , Monad
--     , MonadReader AppEnv
--     , MonadIO
--     , MonadUnliftIO
--     , MonadLogger
--     )


-- instance MonadIO m => IdService (IdSvcIoNewtypeWrap m) where
--   nextCargoId = liftIO $ do
--     putStrLn "Getting new ID"
--     threadDelay 2_000_000
--     Right . CargoId <$> nextRandom
-----------------------------------------------------------------------

newtype IdSvcIo m a = IdSvcIo (m a)
  deriving newtype (
      Functor 
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    )
instance MonadIO m => IdService (IdSvcIo m) where
  nextCargoId = liftIO $ do
    putStrLn "Getting new ID"
    threadDelay 2_000_000
    Right . CargoId <$> nextRandom

  {-# INLINE nextCargoId #-}


