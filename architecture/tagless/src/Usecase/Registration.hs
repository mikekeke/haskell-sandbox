{-# LANGUAGE ScopedTypeVariables #-}

module Usecase.Registration where

import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Either (EitherT, firstEitherT, newEitherT, runEitherT)
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
  | FailedToGetNewId IdServiceError
  | UserRepoError UserRepoError
  deriving stock (Show)

registerCargo ::
  ( Monad m,
    MonadUnliftIO m,
    CargoRegistry m,
    IdService m,
    UserRegistry m,
    MonadLogger m
  ) =>
  Person ->
  Goods ->
  m (Either RegistrationError ())
registerCargo p gs = do
  (uid, user) <-
    runConcurrently $
      (,) <$> Concurrently nextCargoId <*> Concurrently registerUser
  -- logDebugN $ "@@NEW ID: " <> show uid
  runEitherT $ do
    newId <- handling FailedToGetNewId (pure uid) -- FIXME: refactor to better solution
    user <- handling UserRepoError (pure user)
    let newCargo = Cargo newId p gs
    handling CargoRepoErr $ addCargo user newCargo
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