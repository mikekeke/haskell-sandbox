module App.Types where
import Database.SQLite.Simple (Connection)
import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, LoggingT, runStdoutLoggingT)
import Repos (IdService)
import App.HttpIdService ( HttpIdService(HttpIdService) )
import App.IdServiceSubs ( IdSvcIo(IdSvcIo) )

run :: MonadIO m => App m a -> AppEnv -> m a
run app env = 
  runStdoutLoggingT 
  $ flip runReaderT env 
  $ unIOApp app


newtype App m a = App {unIOApp :: ReaderT AppEnv (LoggingT m) a}
  deriving newtype (
      Functor 
    , Applicative
    , Monad
    , MonadReader AppEnv
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    )
  -- deriving IdService via (HttpIdService (App m)) -- deriving through another instance (implementation)
  deriving IdService via (IdSvcIo (App m)) -- TODO

  -- deriving IdService via (IdSvcIoNewtypeWrap m) -- deriving via newtype wrapper
  {- ^^^ can't do like this and keep IdSvcIoNewtypeWrap in another module
     coz of circular imports.
     But can do standalone deriving - see "Assembling variants" in `App.hs`
  -}


-- newtype HttpIdService m a = IoIdSvcIoWithDelay (m a)
--   deriving newtype (
--       Functor 
--     , Applicative
--     , Monad
--     -- , MonadReader AppEnv
--     , MonadIO
--     , MonadUnliftIO
--     -- , MonadLogger
--     )
-- instance MonadIO m =>IdService (HttpIdService m) where
--   nextCargoId = liftIO $ do
--     putStrLn "Getting new ID"
--     threadDelay 2_000_000
--     Right . CargoId <$> nextRandom


data AppEnv = AppEnv
  { dbConnPath :: String

  }

-- data DbEnv = DbEnv
--   {
--   }
data AppConfig = AppConfig


