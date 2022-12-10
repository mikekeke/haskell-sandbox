{-# LANGUAGE ScopedTypeVariables #-}

module Usecase.Registration where

import Control.Monad.Logger (MonadLogger, logErrorN, logInfoN)
import Control.Monad.Trans.Either (EitherT, firstEitherT, newEitherT, runEitherT)
import Repos
  ( CargoRegistry (..),
    CargoRegistryError,
    IdService (..),
    IdServiceError,
    UserRegistry (addUser),
    UserRepoError (UserNotFound),
  )
import Types (Cargo (Cargo), Goods, Person, userId)
import UnliftIO (MonadUnliftIO, conc, runConc)
import Control.Arrow (left)

-- import Users (UserServiceError, registerUser)

data RegistrationError
  = FailedToRegisterCargo CargoRegistryError
  | FailedToGetNewId IdServiceError
  | FailedRegisterUser UserRepoError
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
  res <- runConc $ (,) <$> conc getNewId <*> conc registerUser
  withLogging $ runEitherT $ do
    (newId, user) <- hoistEither $ uncurry (liftA2 (,)) res -- FIXME: ugly, neponyatno
    let newCargo = Cargo newId (userId user) gs
    addNewCargo newCargo
  where
    getNewId = left FailedToGetNewId <$> nextCargoId
    registerUser = left FailedRegisterUser <$> addUser p 
    addNewCargo newCargo = addCargo newCargo `handling` FailedToRegisterCargo

    withLogging :: 
      MonadLogger m => 
      m (Either RegistrationError ()) ->
      m (Either RegistrationError ())
    withLogging act = do
      logInfoN "registering new cargo"
      res <- act 
      either
        (logErrorN . show)
        (const $ logInfoN "New cargo registered")
        res
      return  res

handling :: Functor m => m (Either e a) -> (e -> e') -> EitherT e' m a
handling m errCons = firstEitherT errCons $ newEitherT m


