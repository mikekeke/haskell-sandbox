{-# OPTIONS_GHC -Wno-orphans #-}
module App.CargoRegistry where

import Data.UUID.V4 (nextRandom)
import Repos (CargoRegistry (addCargo, allCargos))
import Types (Cargo (Cargo), CargoId (CargoId), Goods (Goods), Person (Person))
import App.Types (App)
import UnliftIO (MonadUnliftIO)

instance MonadUnliftIO m => CargoRegistry (App m) where
  addCargo _c =
    liftIO $ do
      putStrLn "Adding cargo"
      Right <$> putStrLn "Cargo added"

  allCargos =
    liftIO $ do
      putStrLn "Getting all cargos"
      uid <- CargoId <$> nextRandom
      pure . Right $ [Cargo uid (Person "TestName" "test-phone-123") (Goods ["g1", "g2"])]