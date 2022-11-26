module App.Types where
import Database.SQLite.Simple (Connection)
import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, LoggingT, runStdoutLoggingT)

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
  -- deriving IdService via (IdSvcIoWithDelayImpl m)
  {- ^^^ can't do like this and keep IdSvcIoWithDelayImpl in another module
     coz of circular imports.
     But can do standalone deriving in `App.hs`
  -}

data AppEnv = AppEnv
  { dbConn :: Connection
  }

-- data DbEnv = DbEnv
--   {
--   }
data AppConfig = AppConfig


