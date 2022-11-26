module Repos where

import Data.UUID (UUID)

import Types ( CargoId, Cargo, Person, UserPhone, User )

-- Cargo repo --
data CargoRegistryError 
  = SomeCargoRegError String
  deriving stock Show

class Monad m => CargoRegistry m where
  addCargo :: Cargo -> m (Either CargoRegistryError ())
  allCargos :: m (Either CargoRegistryError [Cargo])

-- ID service --
data IdServiceError
  = SomeIdServiceErr String
  deriving stock Show

class Monad m => IdService m where
  nextCargoId :: m (Either IdServiceError CargoId)

-- Users repo --

data UserRepoError 
  = UserNotFound UserPhone
  | ManyUsersFound UserPhone
  | OtherUSerRepoErr String
  deriving stock Show

class Monad m => UserRegistry m where
  addUser :: Person -> m (Either UserRepoError User)
  getUser :: UserPhone -> m (Either UserRepoError User)
  allUsers :: m (Either UserRepoError [User])
