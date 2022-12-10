{-# OPTIONS_GHC -Wno-orphans #-}

module App.CargoRegistry where

import App.Types (App, AppEnv (dbConn))
import Control.Exception (try)
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (SQLError, execute)
import Repos (CargoRegistry (addCargo, allCargos), CargoRegistryError (SomeCargoRegError))
import Types (Cargo (Cargo), CargoId (CargoId), Goods (Goods))
import App.SQLiteInstances ()
import Control.Arrow (left)

instance MonadIO m => CargoRegistry (App m) where
  addCargo cargo = do
    conn <- asks dbConn
    tryAdd conn
      <&> left (SomeCargoRegError . show)
    where
      tryAdd c = liftIO $
        try @SQLError $ do
          execute c "INSERT INTO cargo (id, owner_id, goods) VALUES (?,?,?)" cargo

  allCargos =
    liftIO $ do
      putStrLn "Getting all cargos"
      uid <- CargoId <$> nextRandom
      pure . Right $ [Cargo uid "test-phone-123" (Goods ["g1", "g2"])]

  {-# INLINE addCargo #-}
  {-# INLINE allCargos #-}