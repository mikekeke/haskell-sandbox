module Main where

import Data.Char
import App
import Usecase.Registration qualified as Reg
import Types
import Repos (UserRegistry(allUsers), CargoRegistry (allCargos), CargoRegistryError, IdService, UserRepoError)
import UnliftIO (MonadUnliftIO)
import Text.Show.Pretty (pPrint)




main :: IO ()
main = do
  putStrLn $ replicate 50 '*'
  let conf = App.AppConfig
  env <- App.setup conf
  res <- App.run someApp env
  pPrint res

someApp ::
  (MonadUnliftIO m, CargoRegistry m, IdService m, UserRegistry m)
   => m (Either UserRepoError [User], Either CargoRegistryError [Cargo])
someApp = do
  _ <- Reg.registerCargo (Person "Bob" "3344") (Goods ["Bob's shit"])
  _ <- Reg.registerCargo (Person "Tom" "22111") (Goods ["bread", "pitt"])
  (,) <$> allUsers <*> allCargos

withCapitalizer :: ((String -> String) -> IO ()) -> IO ()
withCapitalizer act = do
  act (map toUpper)
