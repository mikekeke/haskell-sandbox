{-# OPTIONS_GHC -Wno-orphans #-}
module App
  ( setup
  , module X
  )
where

import Database.SQLite.Simple (execute_, open)
import App.UserRegistry as X
import App.IdServiceNewtypeWrap as X
import App.CargoRegistry as X ()
import App.Types as X



bpPath :: String
bpPath = "app.db"

setup :: AppConfig -> IO AppEnv
setup _ = do
  conn <- open bpPath
  execute_ conn "CREATE TABLE IF NOT EXISTS user (name TEXT, phone TEXT not null unique)"
  return (AppEnv conn)

-------------------------
-- Assembling variants --
-------------------------

{- Can use it to wrap original whole `App` type with newtype and avoid cyclic
   dependency in case of "deriving via after `App` type definition like
   `deriving IdService via (IdSvcIoNewtypeWrap m)` - see `App` definition in `App.Types`
-}
-- deriving via (IdSvcIoNewtypeWrap  m) 
--   instance MonadIO m => (IdService (App m)) 

