module IOApp.Types where
import Control.Monad.Reader (ReaderT, MonadReader, MonadIO)
import Database.SQLite.Simple (Connection)
import UnliftIO (MonadUnliftIO)

newtype IOApp a = IOApp {unIOApp :: ReaderT IOAppEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader IOAppEnv, MonadIO, MonadUnliftIO)

data IOAppEnv = IOAppEnv
  { dbConn :: Connection
  }

-- data DbEnv = DbEnv
--   {
--   }
data IOAppConfig = IOAppConfig
