{-# LANGUAGE ScopedTypeVariables #-}

module Usecase.Registration where

-- import Control.Monad.Trans.Either

import Control.Arrow (left)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans.Either (EitherT, firstEitherT, newEitherT, runEitherT)

-- import UnliftIO.Async (Concurrently (Concurrently, runConcurrently))
-- import UnliftIO.Async (Concurrently (Concurrently, runConcurrently))
import Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, newEitherT, runEitherT)
import Debug.Trace
import Repos
  ( CargoRegistry (..),
    CargoRegistryError,
    IdService (..),
    IdServiceError,
    UserRegistry (addUser, getUser),
    UserRepoError (UserNotFound),
  )
import Types (Cargo (Cargo), Goods, Person (phone))
import UnliftIO (Concurrently (Concurrently, runConcurrently), MonadUnliftIO)

-- import Users (UserServiceError, registerUser)

data RegistrationError
  = CargoRepoErr CargoRegistryError
  | IdServiceErr IdServiceError
  | UserRepoError UserRepoError
  deriving stock (Show)

registerCargo ::
  (Monad m, MonadUnliftIO m, CargoRegistry m, IdService m, UserRegistry m) =>
  Person ->
  Goods ->
  m (Either RegistrationError ())
registerCargo p gs = do
  (uid, user) <-
    runConcurrently $
      (,) <$> Concurrently nextCargoId <*> Concurrently registerUser
  runEitherT $ do
    newId <- handling IdServiceErr (pure uid)
    userReg <- handling UserRepoError (pure user)
    let newCargo = Cargo newId p gs
    handling CargoRepoErr $ addCargo newCargo
  where
    registerUser = do
      eUser <- getUser (phone p)
      case eUser of
        Right u -> pure $ Right u
        Left (UserNotFound _) -> addUser p
        Left e -> pure $ Left e

handling ::
  Monad m =>
  (e -> RegistrationError) ->
  m (Either e a) ->
  EitherT RegistrationError m a
handling errorCons action =
  firstEitherT errorCons $ newEitherT action