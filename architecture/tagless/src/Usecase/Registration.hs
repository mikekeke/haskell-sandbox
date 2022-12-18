
module Usecase.Registration where

import Control.Arrow (left)
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
import Types (Cargo (Cargo), CargoId (CargoId), Goods, Person, User, userId)
import UnliftIO (MonadUnliftIO, conc, runConc)

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
  withLogging $
    runEitherT $ do
      (newId, user) <- asyncGetIdForCargoAndRegisterUser
      let newCargo = Cargo newId (userId user) gs
      addNewCargo newCargo
  where
    addNewCargo newCargo = addCargo newCargo `handling` FailedToRegisterCargo
    
    -- FIXME: if uncomment "because type variable ‘m1’ would escape its scope"
    -- asyncGetIdForCargoAndRegisterUser :: EitherT RegistrationError m (CargoId, User)
    asyncGetIdForCargoAndRegisterUser =
      newEitherT $
        uncurry (liftA2 (,))
          <$> runConc ((,) <$> conc getNewId <*> conc registerUser)

    getNewId = left FailedToGetNewId <$> nextCargoId

    registerUser = do
      left FailedRegisterUser <$> addUser p

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
      return res

handling :: Functor m => m (Either e a) -> (e -> e') -> EitherT e' m a
handling m errCons = firstEitherT errCons $ newEitherT m
