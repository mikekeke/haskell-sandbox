module IOApp
  ( setup
  , run
  , module X
  )
where

import Control.Arrow (left)
import Control.Exception (IOException, try)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT), asks, runReader)
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Database.SQLite.Simple (Connection, SQLError, execute, execute_, open, query_)
import IOApp.CargoRegistry as X
import IOApp.UserRegistry as X
import IOApp.IdService as X
import IOApp.Types as X
import Repos (UserRegistry (addUser, allUsers, getUser), UserRepoError (OtherUSerRepoErr, UserNotFound))
import Types (Person (Person))
import IOApp.Types (IOAppConfig, IOAppEnv (IOAppEnv), IOApp (unIOApp))



bpPath :: String
bpPath = "app.db"

setup :: IOAppConfig -> IO IOAppEnv
setup _ = do
  conn <- open bpPath
  execute_ conn "CREATE TABLE IF NOT EXISTS user (name TEXT, phone TEXT not null unique)"
  return (IOAppEnv conn)

run = runReaderT . unIOApp
