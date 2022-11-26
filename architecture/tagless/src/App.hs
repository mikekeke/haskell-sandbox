{-# OPTIONS_GHC -Wno-orphans #-}
module App
  ( setup
  , module X
  )
where

import Database.SQLite.Simple (execute_, open)
import App.UserRegistry as X
import App.IdService as X
-- import App.CargoRegistry  as X
import App.Types as X
import Repos (IdService)



bpPath :: String
bpPath = "app.db"

setup :: AppConfig -> IO AppEnv
setup _ = do
  conn <- open bpPath
  execute_ conn "CREATE TABLE IF NOT EXISTS user (name TEXT, phone TEXT not null unique)"
  return (AppEnv conn)

deriving via (IdSvcIoWithDelayImpl  m) 
  instance MonadIO m => (IdService (App m)) 