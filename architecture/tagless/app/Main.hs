{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main where

import App
import Control.Monad.Logger (MonadLogger)
import Data.Char
import Repos
  ( CargoRegistry (allCargos),
    CargoRegistryError,
    IdService,
    UserRegistry (allUsers, getUser),
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
  , usersRes :: Either UserRepoError [User]
  , cargosRes :: Either CargoRegistryError [Cargo]
  , userRes :: Either UserRepoError User
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
      [ (Person "Bob" "phone-3344", Goods ["Bob's shit"]),
        (Person "Bob" "phone-3344", Goods ["MORE Bob's shit"]),
        (Person "Tom" "phone-111", Goods ["bread", "pitt"]),
        (Person "Lol" "phone-3344", Goods ["some", "stuff"])
      ]
  users <- allUsers
  cargos <- allCargos 
  user <- getUser "3344"
  pure $
    SomeAppRes
      regRes
      users
      cargos
      user

withCapitalizer :: ((String -> String) -> IO ()) -> IO ()
withCapitalizer act = do
  act (map toUpper)

