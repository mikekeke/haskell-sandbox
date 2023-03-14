{-# OPTIONS_GHC -Wno-orphans #-}

module App.CargoRegistry where

import App.Types (App, AppEnv (dbConnPath))
import Control.Exception (try)
import Database.SQLite.Simple (SQLError, execute, query_, withConnection)
import Repos (CargoRegistry (addCargo, allCargos), CargoRegistryError (SomeCargoRegError))
import App.SQLiteInstances ()
import Control.Arrow (left)

instance MonadIO m => CargoRegistry (App m) where
  addCargo cargo = do
    path <- asks dbConnPath
    liftIO $ withConnection path $ \ conn ->
      tryRun conn <&> left (SomeCargoRegError . show)
    where
      tryRun c =
        try @SQLError $ do
          execute c "INSERT INTO cargo (id, owner_id, goods) VALUES (?,?,?)" cargo

  allCargos = do
    path <- asks dbConnPath
    liftIO $ withConnection path $ \ conn ->
      tryRun conn <&> left (SomeCargoRegError . show)
    where
      tryRun c = liftIO $
        try @SQLError $ do
          query_ c "SELECT * FROM cargo"

  {-# INLINE addCargo #-}
  {-# INLINE allCargos #-}