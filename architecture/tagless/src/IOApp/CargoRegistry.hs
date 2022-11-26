module IOApp.CargoRegistry where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Repos (CargoRegistry (addCargo, allCargos))
import Types (Cargo (Cargo), CargoId (CargoId), Goods (Goods), Person (Person))
import IOApp.Types (IOApp)

instance CargoRegistry IOApp where
  addCargo _c =
    liftIO $ do
      putStrLn "Adding cargo"
      Right <$> putStrLn "Cargo added"

  allCargos =
    liftIO $ do
      putStrLn "Getting all cargos"
      uid <- CargoId <$> nextRandom
      pure . Right $ [Cargo uid (Person "TestName" "test-phone-123") (Goods ["g1", "g2"])]