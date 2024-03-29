{-# OPTIONS_GHC -Wno-orphans #-}
module App
  ( setup
  , module X
  )
where

import Database.SQLite.Simple (execute_, open, withConnection)
import App.UserRegistry as X
import App.IdServiceSubs as X
import App.CargoRegistry as X ()
import App.Types as X



bpPath :: String
bpPath = "app.db"

setup :: AppConfig -> IO AppEnv
setup _ = do
  withConnection bpPath $ \conn ->
    traverse_ ($ conn) 
      [ initUserTable
      , initCargoTable
      ]
    
  return (AppEnv bpPath)
  where
    initUserTable conn =
      execute_ conn
        "CREATE TABLE IF NOT EXISTS user (id TEXT not null unique, name TEXT, phone TEXT)"

    initCargoTable conn =
      execute_ conn
        "CREATE TABLE IF NOT EXISTS cargo (id TEXT not null unique, owner_id TEXT, goods TEXT)"

-------------------------
-- Assembling variants --
-------------------------

{- Can use it to wrap original whole `App` type with newtype and avoid cyclic
   dependency in case of "deriving via after `App` type definition like
   `deriving IdService via (IdSvcIoNewtypeWrap m)` - see `App` definition in `App.Types`
-}
-- deriving via (IdSvcIoNewtypeWrap  m) 
--   instance MonadIO m => (IdService (App m)) 

