module Main where

import App
import Control.Monad.Logger (MonadLogger)
import Data.Char
import Repos
  ( CargoRegistry (allCargos),
    CargoRegistryError,
    IdService,
    UserRegistry (allUsers),
    UserRepoError,
  )
import Text.Show.Pretty (pPrint)
import Types
import UnliftIO (MonadUnliftIO)
import Usecase.Registration (RegistrationError)
import Usecase.Registration qualified as Reg

main :: IO ()
main = do
  putStrLn $ replicate 50 '*'
  let conf = App.AppConfig
  env <- App.setup conf
  res <- App.run someApp env
  pPrint res

data SomeAppRes = SomeAppRes
  { regResults :: [Either RegistrationError ()]
  }
  deriving stock (Show)

someApp ::
  ( MonadUnliftIO m,
    CargoRegistry m,
    IdService m,
    UserRegistry m,
    MonadLogger m
  ) =>
  m SomeAppRes
someApp = do
  regRes <-
    traverse
      (uncurry Reg.registerCargo)
      [ (Person "Bob" "3344", Goods ["Bob's shit"]),
        (Person "Tom" "22111", Goods ["bread", "pitt"])
      ]
  pure $
    SomeAppRes
      regRes

withCapitalizer :: ((String -> String) -> IO ()) -> IO ()
withCapitalizer act = do
  act (map toUpper)
