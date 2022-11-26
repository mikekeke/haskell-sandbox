module Main where

import Control.Monad.IO.Class
import Data.Char
import Data.IORef
import IOApp qualified
import Usecase.Registration qualified as Reg
import SQLite.DebugSQLite qualified as DSQL
import TestApp qualified as TA
import Text.Show.Pretty (ppShow)
import Types
import Text.Show.Pretty
import Repos (UserRegistry(allUsers), CargoRegistry (allCargos))

main :: IO ()
main = do
  putStrLn $ replicate 50 '*'
  let conf = IOApp.IOAppConfig
  res <- IOApp.setup conf >>= IOApp.run someApp
  pPrint res

  -- res <- TA.runTa someApp
  -- putStrLn $ ppShow res
-- withCapitalizer $ \upperer -> putStrLn (upperer "test")

-- app =
--   Users.registerUser (Person "Bob" "3344")
--     >>= either (pure . Left) (const Users.contragents)

-- someApp :: Monad m => m [RegisteredCargo]
someApp = do
  Reg.registerCargo (Person "Bob" "3344") (Goods ["Bob's shit"])
  -- Reg.registerCargo (Person "Tom" "22111") (Goods ["bread", "pitt"])
  (,) <$> allUsers <*> allCargos 

withCapitalizer :: ((String -> String) -> IO ()) -> IO ()
withCapitalizer act = do
  act (map toUpper)
